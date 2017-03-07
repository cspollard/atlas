{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Codec.Compression.GZip   (decompress)
import           Control.Lens
import qualified Data.ByteString.Lazy     as BS
import qualified Data.IntMap.Strict       as IM
import qualified Data.Map.Strict          as M
import           Data.Maybe               (fromMaybe)
import           Data.Monoid
import           Data.Serialize           (decodeLazy)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           Options.Applicative
import           System.IO                (hFlush, stdout)


import           Data.Atlas.CrossSections
import           Data.Weighted
import           Data.YODA.Obj

data InArgs =
  InArgs
    { outfolder :: String
    , xsecfile  :: String
    , lumi      :: Double
    , infiles   :: [String]
    }

inArgs :: Parser InArgs
inArgs =
  InArgs
    <$> strOption
      ( long "outfolder"
      <> metavar "OUTFOLDER" )
    <*> strOption
      ( long "xsecfile"
      <> metavar "XSECFILE" )
    <*> option auto
      ( long "lumi"
      <> metavar "LUMI" )
    <*> some (strArgument (metavar "INFILES"))

opts :: ParserInfo InArgs
opts = info (helper <*> inArgs) fullDesc


main :: IO ()
main = do
  args <- execParser opts

  xsecs <-
    fromMaybe (error "failed to parse xsec file.")
      <$> (fmap.fmap.fmap) fst (readXSecFile (xsecfile args))

  im <-
    foldl (IM.unionWith mergeRuns) IM.empty
      <$> mapM decodeFile (infiles args)

  let im' =
        flip IM.mapWithKey im $
          \ds (n, hs) ->
            if ds == 0
              then
                hs & traverse.annots.at "LineStyle" ?~ "none"
                  & traverse.annots.at "LineColor" ?~ "Black"
                  & traverse.annots.at "DotSize" ?~ "0.1"
                  & traverse.annots.at "ErrorBars" ?~ "1"
                  & traverse.annots.at "PolyMarker" ?~ "*"
              else
                let w = lumi args * (xsecs IM.! ds) / n
                in over (traverse.noted._H1DD.histVals) (scaling w) hs

  iforM_ im'
    $ \ds hs ->
      T.writeFile
        (outfolder args ++ '/' : show ds ++ ".yoda")
        (T.unlines . M.elems $ M.mapWithKey printYodaObj hs)

mergeRuns :: (Double, YodaFolder) -> (Double, YodaFolder) -> (Double, YodaFolder)
mergeRuns (sumwgt, hs) (sumwgt', hs') =
    let s = sumwgt + sumwgt'
        yf = mergeYF hs hs'
    in s `seq` yf `seq` (s, yf)


decodeFile :: String -> IO (IM.IntMap (Double, YodaFolder))
decodeFile f = do
    putStrLn ("decoding file " ++ f) >> hFlush stdout
    eim <- decodeLazy . decompress <$> BS.readFile f ::
                IO (Either String (IM.IntMap (Double, YodaFolder)))

    case eim of
         Left _   -> error $ "failed to decode file " ++ f
         Right im -> return im
