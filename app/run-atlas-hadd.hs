{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Atlas
import           Data.Monoid
import           Options.Applicative
import           System.IO

data InArgs =
  InArgs
    { outfile :: String
    , regex   :: Maybe String
    , infiles :: [String]
    }

inArgs :: Parser InArgs
inArgs =
  InArgs
  <$> strOption
    ( long "outfile" <> short 'o' <> metavar "OUTFILE" )
  <*> optional
    ( strOption (long "regex" <> metavar "REGEX=\".*\"") )
  <*> some (strArgument (metavar "INFILES"))

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- execParser $ info (helper <*> inArgs) fullDesc

  efol <- decodeFiles' (regex args) (infiles args)
  case efol of
    Left err  -> error err
    Right fol -> encodeFile (outfile args) fol
