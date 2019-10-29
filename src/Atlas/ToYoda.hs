{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Atlas.ToYoda where

import Both
import           Atlas
import           Atlas.Streaming
import           Atlas.CrossSections
import           Control.Lens
import           Data.Maybe          (fromMaybe)
import           Data.Semigroup      (Sum (..), (<>))
import           Options.Applicative
import Data.Annotated
import Data.StrictMap
import System.IO


type ProcMap = StrictMap ProcessInfo
type ProcSums = Both (Sum Double) AnaObjs


data InArgs
  = InArgs
  { outfolder :: String
  , xsecfile  :: String
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
  <*> some (strArgument (metavar "INFILES"))


opts :: ParserInfo InArgs
opts = info (helper <*> inArgs) fullDesc


mainWith
  :: (String -> ProcMap AnaObjs -> IO ()) -> IO ()
mainWith writeFiles = do
  hSetBuffering stdout LineBuffering
  args <- execParser opts

  xsecs <-
    fromMaybe (error "failed to parse xsec file.")
      <$> (fmap.fmap.fmap) fst (readXSecFile (xsecfile args))

  pm <- decodeFiles $ infiles args

  let (procmap' :: ProcMap AnaObjs) =
        flip imap pm
        $ \proci (Both (Sum w) aos) ->
          let dsid' = dsid proci
          in if dsid' == 0
              then bimap dataStyle dataStyle aos
              else
                let t = ("\"" <> processTitle proci <> "\"")
                    totweight = (xsecs ^?! ix dsid') / w
                in bimap (mcStyle totweight t) (mcStyle totweight t) aos

  writeFiles (outfolder args) procmap'


  where
    dataStyle =
      over (traverse.notes)
      $ (at "LineStyle" ?~ "solid")
        . (at "LineColor" ?~ "Black")
        . (at "DotSize" ?~ "0.15")
        . (at "ErrorBars" ?~ "1")
        . (at "PolyMarker" ?~ "*")
        . (at "Title" ?~ "\"data\"")


    mcStyle w t =
      fmap
      $ over noted ((fmap.fmap.fmap) (*w))
        . over notes (at "Title" ?~ t)
