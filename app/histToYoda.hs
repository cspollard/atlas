{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens
import Control.Monad ((>=>))
import Control.Applicative (ZipList(..), liftA2)

import qualified Data.ByteString.Lazy as BS
import Data.Serialize (decodeLazy)
import Data.Serialize.ZipList ()
import Codec.Compression.GZip (decompress)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.IntMap as IM
import Data.Maybe (fromMaybe)

import Data.YODA.Obj
import Data.Atlas.CrossSections

import Options.Generic

data Args = Args { outfolder :: String
                 , infiles :: [String]
                 , xsecfile :: String
                 , lumi :: Double
                 } deriving (Show, Generic)

instance ParseRecord Args where

main :: IO ()
main = do
    args <- getRecord "histToYoda" :: IO Args

    xsecs <- fromMaybe (error "failed to parse xsec file.")
                <$> (fmap.fmap.fmap) fst (readXSecFile (xsecfile args))

    im <- foldl (IM.unionWith mergeRuns) IM.empty
            <$> mapM decodeFile (infiles args)

    let im' = flip IM.mapWithKey im $
                \ds (n, hs) -> if ds < 300000
                                then hs
                                else over (traverse.noted._H1DD) (scaling $ lumi args * (xsecs IM.! ds) / n) hs

    iforM_ im' $
        \ds hs -> T.writeFile (outfolder args ++ '/' : show ds ++ ".yoda")
                    (T.unlines $ hs ^.. traverse.to printYObj)

mergeRuns :: (Double, ZipList YodaObj) -> (Double, ZipList YodaObj) -> (Double, ZipList YodaObj)
mergeRuns (sumwgt, hs) (sumwgt', hs') = (sumwgt+sumwgt', liftA2 mergeYO hs hs')

decodeFile :: String -> IO (IM.IntMap (Double, ZipList YodaObj))
decodeFile f = do
    eim <- decodeLazy . decompress <$> BS.readFile f ::
                IO (Either String (IM.IntMap (Double, ZipList YodaObj)))

    case eim of
         Left _ -> error $ "failed to decode file " ++ f
         Right im -> return im
