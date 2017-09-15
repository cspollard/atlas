{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Atlas.ToYoda
  ( scaleH, decodeFile, decodeFiles
  , ProcMap, mainWith
  ) where

import           Atlas
import           Atlas.CrossSections
import           Atlas.ProcessInfo
import           Control.Lens
import qualified Data.IntMap.Strict  as IM
import           Data.Maybe          (fromMaybe)
import           Data.Semigroup      (Sum (..), (<>))
import           Options.Applicative


type ProcMap = StrictMap ProcessInfo

scaleH :: Double -> Obj -> Obj
scaleH x (H1DD h) = H1DD $ scaling x h
scaleH x (P1DD h) = P1DD $ scaling x h
scaleH x (H2DD h) = H2DD $ scaling x h


data InArgs =
  InArgs
    { outfolder :: String
    , xsecfile  :: String
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
  <*> optional
    ( strOption (long "regex" <> metavar "REGEX=\".*\"") )
  <*> some (strArgument (metavar "INFILES"))


opts :: ParserInfo InArgs
opts = info (helper <*> inArgs) fullDesc


mainWith
  :: (String -> ProcMap (Folder (Vars YodaObj)) -> IO ()) -> IO ()
mainWith writeFiles = do
  args <- execParser opts

  xsecs <-
    fromMaybe (error "failed to parse xsec file.")
      <$> (fmap.fmap.fmap) fst (readXSecFile (xsecfile args))

  epm <- decodeFiles (regex args) (infiles args)
  case epm of
    Left err -> error err
    Right procmap -> do
      -- TODO
      -- so much traverse.....
      let procmap' =
            flip imap procmap
            $ \proci (Sum w, hs) ->
              if dsid proci == 0
                then
                  hs
                    & over (traverse.traverse.annots)
                      ( (at "LineStyle" ?~ "solid")
                      . (at "LineColor" ?~ "Black")
                      . (at "DotSize" ?~ "0.15")
                      . (at "ErrorBars" ?~ "1")
                      . (at "PolyMarker" ?~ "*")
                      . (at "Title" ?~ "\"data\"")
                      )
                else
                  let t = ("\"" <> processTitle proci <> "\"")
                  in hs
                      & over
                        (traverse.traverse)
                        ( over noted (scaleH (xsecs IM.! dsid proci / w))
                          . set (annots.at "Title") (Just t)
                        )

      writeFiles (outfolder args) procmap'
