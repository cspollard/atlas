{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Atlas.ToYoda where

import           Atlas
import           Atlas.CrossSections
import           Atlas.ProcessInfo
import           Codec.Compression.GZip (decompress)
import           Control.Lens
import qualified Data.ByteString.Lazy   as BS
import qualified Data.IntMap.Strict     as IM
import qualified Data.Map.Strict        as M
import           Data.Maybe             (fromMaybe)
import           Data.Monoid
import           Data.Serialize
import qualified Data.Text              as T
import           Options.Applicative
import           Pipes                  ((<-<))
import qualified Pipes                  as P
import qualified Pipes.ByteString       as PBS
import qualified Pipes.Prelude          as P
import           System.IO              (hFlush, stdout)

type ProcMap = IM.IntMap

scaleH :: Double -> Obj -> Obj
scaleH x (H1DD h) = H1DD $ scaling x h
scaleH x (P1DD h) = P1DD $ scaling x h
scaleH x (H2DD h) = H2DD $ scaling x h

data InArgs =
  InArgs
    { outfolder :: String
    , xsecfile  :: String
    , lumi      :: Double
    , regex     :: Maybe String
    , infiles   :: [String]
    }

inArgs :: Parser InArgs
inArgs = InArgs
  <$> strOption
    ( long "outfolder"
    <> short 'o'
    <> metavar "OUTFOLDER" )
  <*> strOption
    ( long "xsecfile"
    <> metavar "XSECFILE" )
  <*> option auto
    ( long "lumi"
    <> metavar "LUMI" )
  <*> optional
    ( strOption (long "regex" <> metavar "REGEX=\".*\"") )
  <*> some (strArgument (metavar "PREDFILES"))

opts :: ParserInfo InArgs
opts = info (helper <*> inArgs) fullDesc

-- TODO
-- partial!
variationFromMap :: Ord k => k -> M.Map k a -> Variations k a
variationFromMap k m =
  let n = m M.! k
  in Variations n $ sans k m

mainWith
  :: (Double -> String -> ProcMap (Folder (Vars YodaObj)) -> IO ()) -> IO ()
mainWith writeFiles = do
  args <- execParser opts

  xsecs <-
    fromMaybe (error "failed to parse xsec file.")
      <$> (fmap.fmap.fmap) fst (readXSecFile (xsecfile args))

  let f = decodeFile xsecs . fromMaybe "*" $ regex args
  procmap <- P.foldM (\x fn -> IM.union x <$> f fn) (return IM.empty) return (P.each $ infiles args)

  let procmap' =
        flip IM.mapWithKey procmap
        $ \ds hs ->
          if ds == 0
            then
              hs
                & traverse.traverse.annots.at "LineStyle" ?~ "solid"
                & traverse.traverse.annots.at "LineColor" ?~ "Black"
                & traverse.traverse.annots.at "DotSize" ?~ "0.15"
                & traverse.traverse.annots.at "ErrorBars" ?~ "1"
                & traverse.traverse.annots.at "PolyMarker" ?~ "*"
                & traverse.traverse.annots.at "Title" ?~ "\"data\""
            else
              hs
                & traverse.traverse.annots.at "Title"
                  ?~ ("\"" <> processTitle ds <> "\"")

  writeFiles (lumi args) (outfolder args) procmap'


dsidOTHER :: Int
dsidOTHER = 999999

decodeFile
  :: IM.IntMap Double
  -> String
  -> String
  -> IO (IM.IntMap (Folder (Vars YodaObj)))
decodeFile xsecs rxp f = do
  putStrLn ("decoding file " ++ f) >> hFlush stdout

  bs <- decompress <$> BS.readFile f

  -- pipes outline:
  -- 1) deserialize into items of type (Text, (Text, (Int, Double, YodaObj)))
  -- 2) cull out those that fail rxp matching
  -- 3) return obj of type Folder (Vars (Int, Double, YodaObj))), which can be
  --      zipped into Folder (Vars YodaObj).

  let filt ((i, _), (t, _)) =
        matchRegex rxp (T.unpack t)
        && processTitle i /= "other"

      prep ((i, d), (t, (t', y))) =
        let y' =
              if i == 0
                then y
                else over noted (scaleH $ (xsecs IM.! i)/d) y
        in (First (Just i), M.singleton t $ M.singleton t' y')

  P.fold (liftA2 $ M.unionWith M.union) (First Nothing, M.empty)
    (\(ds, fol) ->
      case ds of
        First Nothing -> error "no objects were loaded...?"
        First (Just i) -> IM.singleton i $ Folder $ variationFromMap "nominal" <$> fol
    )
    $ P.map prep
      <-< P.filter filt
      <-< deserializer
      <-< PBS.fromLazy bs

-- | De-serialize data from strict 'ByteString's.  Uses @cereal@'s
-- incremental 'Data.Serialize.Get' parser.
deserializer :: (Serialize a, Monad m) => P.Pipe PBS.ByteString a m ()
deserializer = loop Nothing Nothing
  where
    loop mk mbin = do
        bin <- maybe P.await return mbin
        case fromMaybe (runGetPartial get) mk bin of
          Fail reason _leftover ->
              fail reason
          Partial k ->
              loop (Just k) Nothing
          Done c bin' -> do
              P.yield c
              loop Nothing (Just bin')
