{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}

module Data.Atlas.ToYoda where

import           Codec.Compression.GZip    (decompress)
import qualified Control.Foldl             as F
import           Control.Lens
import qualified Data.ByteString.Lazy      as BS
import qualified Data.IntMap.Strict        as IM
import qualified Data.Map.Strict           as M
import           Data.Maybe                (fromMaybe)
import           Data.Monoid
import           Data.Serialize            (decodeLazy)
import qualified Data.Text                 as T
import qualified List.Transformer          as L
import           Options.Applicative
import           System.IO                 (hFlush, stdout)
import           Text.Regex.Base.RegexLike
import           Text.Regex.Posix.String

import           Data.Atlas.CrossSections
import           Data.Atlas.ProcessInfo
import           Data.YODA.Obj

type SystMap = M.Map T.Text
type ProcMap = IM.IntMap

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


mainWith :: (String -> ProcMap (SystMap YodaFolder) -> IO ()) -> IO ()
mainWith writeFiles = do
  args <- execParser opts

  xsecs <-
    fromMaybe (error "failed to parse xsec file.")
      <$> (fmap.fmap.fmap) fst (readXSecFile (xsecfile args))

  let f =
        F.FoldM
          ( \x s ->
            maybe x (\(k, h) -> IM.insertWith (M.unionWith mergeYF) k h x)
              <$> decodeFile xsecs (lumi args) (regex args) s
          )
          (return IM.empty)
          return

  procmap <-
    F.impurely L.foldM f (L.select (infiles args) :: L.ListT IO String)

  let procmap' = flip IM.mapWithKey procmap $
              \ds hs ->
                  if ds == 0
                      then
                        hs
                          & traverse.traverse.annots.at "LineStyle" ?~ "none"
                          & traverse.traverse.annots.at "LineColor" ?~ "Black"
                          & traverse.traverse.annots.at "DotSize" ?~ "0.1"
                          & traverse.traverse.annots.at "ErrorBars" ?~ "1"
                          & traverse.traverse.annots.at "PolyMarker" ?~ "*"
                          & traverse.traverse.annots.at "Title" ?~ "\"data\""
                      else
                        hs
                          & traverse.traverse.annots.at "Title"
                            ?~ ("\"" <> processTitle ds <> "\"")

  writeFiles (outfolder args) procmap'


dsidOTHER :: Int
dsidOTHER = 999999


decodeFile
  :: IM.IntMap Double
  -> Double
  -> Maybe String
  -> String
  -> IO (Maybe (Int, SystMap YodaFolder))
decodeFile xsecs lu rxp f = do
  putStrLn ("decoding file " ++ f) >> hFlush stdout
  e <- decodeLazy . decompress <$> BS.readFile f ::
    IO (Either String (Maybe (Int, Double, SystMap YodaFolder)))

  case e of
    Left _ -> error $ "failed to decode file " ++ f

    Right Nothing -> return Nothing
    Right (Just (dsid, sumwgt, hs)) -> do
      let hs' = filt <$> hs
      return
        $ if processTitle dsid == "other"
          then Nothing
          else Just $
            if dsid == 0
              then (0, over (traverse.noted._H1DD) (scaling $ 1.0/lu) <$> hs')
              else
                ( dsid
                , hs' <&> over (traverse.noted._H1DD) (scaling $ xsecs IM.! dsid/sumwgt)
                )

  where
    filt :: YodaFolder -> YodaFolder
    filt =
      case rxp of
        Nothing -> id
        Just s ->
          M.filterWithKey
            $ \k _ -> matchTest (makeRegex s :: Regex) . T.unpack $ k
