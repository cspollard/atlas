{-# LANGUAGE ScopedTypeVariables  #-}
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
import Data.Annotated
import Both
import Control.Lens (view)
import Data.StrictMap
import Data.StrictHashMap
import Atlas.Streaming
import Data.Bifunctor
import Data.Bitraversable
import Atlas.ToYoda

type FA a = Folder (Annotated a)
type FAV a = Folder (Annotated (Vars a))

main :: IO ()
main = mainWith writeVariations

  where
    writeVariations outf pm =
      iforM_ pm $ \proci aos -> do
        putStrLn "process info:"
        print proci

        let go = sequence . fmap sequence

            aos' :: Vars (Both (FA Histo1D) (FA Histo2D))
            aos' = bitraverse go go aos

        writeVariation (outf ++ "/" ++ show (dsid proci))
          $ variationToMap "nominal" aos'


    writeVariation
      :: String
      -> StrictHashMap T.Text (Both (FA Histo1D) (FA Histo2D))
      -> IO ()
    writeVariation prefix hs =
      iforM_ hs $ \varname (Both h1d h2d) ->
        withFile (prefix ++ "." ++ T.unpack varname ++ ".yoda") WriteMode
        $ \h -> do
          runEffect
            $ each (toList h1d)
              >-> P.map (T.unpack . uncurry printHisto1D)
              >-> P.toHandle h

          runEffect
            $ each (toList h2d)
              >-> P.map (T.unpack . uncurry printHisto2D)
              >-> P.toHandle h


    printHisto1D pa o =
      let h = view noted o
          anns = view notes o
      in T.unlines $
          [ "# BEGIN YODA_HISTO1D " <> pa
          , "Type=Histo1D"
          , "Path=" <> pa
          ]
          ++ fmap (\(k, v) -> k <> "=" <> v) (toList anns)
          ++
            [ T.pack $ printBinned1D printGauss1D h
            , "# END YODA_HISTO1D", ""
            ]

    printHisto2D pa o =
      let h = view noted o
          anns = view notes o
      in T.unlines $
          [ "# BEGIN YODA_HISTO2D " <> pa
          , "Type=Histo2D"
          , "Path=" <> pa
          ]
          ++ fmap (\(k, v) -> k <> "=" <> v) (toList anns)
          ++
            [ T.pack $ printBinned2D printGauss2D h
            , "# END YODA_HISTO2D", ""
            ]
