{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Atlas
import           Control.Lens        (iforM_)
import           Data.Monoid
import qualified Data.Text           as T
import           GHC.Exts
import           Options.Applicative
import           Pipes
import qualified Pipes.Prelude       as P
import           System.IO

data InArgs =
  InArgs
    { outfile :: String
    , regex   :: Maybe String
    , infile  :: String
    }

inArgs :: Parser InArgs
inArgs =
  InArgs
  <$> strOption
    ( long "outfolder" <> short 'o' <> metavar "OUTFOLDER" )
  <*> optional
    ( strOption (long "regex" <> metavar "REGEX=\".*\"") )
  <*> strArgument (metavar "INFILE")

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- execParser $ info (helper <*> inArgs) fullDesc

  efol <- decodeFile (regex args) (infile args)
  case efol of
    Left err  -> error err
    Right (proci, wgt, hs) -> do
      putStrLn "process info:"
      print proci
      putStrLn "sum of weights"
      print wgt
      write (outfile args) . variationToMap "nominal" . sequence $ sequence <$> hs

  where
    write :: String -> VarMap (Folder YodaObj) -> IO ()
    write outf hs =
      iforM_ hs $ \varname hs' ->
        withFile (outf ++ '/' : T.unpack varname ++ ".yoda") WriteMode $ \h ->
          runEffect
          $ each (toList $ _toMap hs')
            >-> P.map (T.unpack . uncurry printYodaObj)
            >-> P.toHandle h
