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
import           Data.Atlas.Variation
import           Data.YODA.Obj

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


mainWith
  :: (Double -> String -> ProcMap (Folder (Vars YodaObj)) -> IO ()) -> IO ()
mainWith writeFiles = do
  args <- execParser opts

  xsecs <-
    fromMaybe (error "failed to parse xsec file.")
      <$> (fmap.fmap.fmap) fst (readXSecFile (xsecfile args))

  procmap <- decodeFiles xsecs (regex args) (L.select $ infiles args)

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

decodeFiles
  :: IM.IntMap Double
  -> Maybe String
  -> L.ListT IO String
  -> IO (IM.IntMap (Folder (Vars YodaObj)))
decodeFiles xss rx infs =
  let f =
        F.FoldM
          ( \x s ->
            maybe x (\(k, h) -> IM.insertWith mappend k h x)
              <$> decodeFile xss rx s
          )
          (return IM.empty)
          return

    in F.impurely L.foldM f infs

decodeFile
  :: IM.IntMap Double
  -> Maybe String
  -> String
  -> IO (Maybe (Int, Folder (Vars YodaObj)))
decodeFile xsecs rxp f = do
  putStrLn ("decoding file " ++ f) >> hFlush stdout
  e <- decodeLazy . decompress <$> BS.readFile f ::
    IO (Either String (Maybe (Int, Double, Folder (Vars YodaObj))))

  case e of
    Left _ -> error $ "failed to decode file " ++ f

    Right Nothing -> return Nothing
    Right (Just (dsid, sumwgt, hs)) -> do
      let hs' = filt hs
      return
        $ if processTitle dsid == "other"
          then Nothing
          else Just $
            if dsid == 0
              then (0, hs')
              else
                ( dsid
                , hs'
                  & over
                    (traverse.traverse.noted)
                    (scaleH $ xsecs IM.! dsid/sumwgt)
                )

  where
    filt :: Folder a -> Folder a
    filt =
      case rxp of
        Nothing -> id
        Just s ->
          inF
            ( M.filterWithKey
              (\k _ -> matchTest (makeRegex s :: Regex) . T.unpack $ k)
            )
