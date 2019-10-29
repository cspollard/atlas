{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Atlas
import           Data.Monoid
import           Options.Applicative
import           System.IO
import qualified Data.ByteString.Lazy as BS
import Atlas.Streaming
import Atlas.ToYoda (ProcMap, ProcSums)

data InArgs
  = InArgs
  { outfile :: String
  , infiles :: [String]
  }


inArgs :: Parser InArgs
inArgs =
  InArgs
  <$> strOption
    ( long "outfile" <> short 'o' <> metavar "OUTFILE" )
  <*> some (strArgument (metavar "INFILES"))


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- execParser $ info (helper <*> inArgs) fullDesc

  (pm :: ProcMap ProcSums) <- decodeFiles $ infiles args

  encodeFile (outfile args) pm
