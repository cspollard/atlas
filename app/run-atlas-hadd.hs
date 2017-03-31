{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Atlas
import           Atlas.ToYoda
import           Codec.Compression.GZip (compress)
import           Control.Lens
import           Control.Monad          (forever)
import qualified Data.ByteString.Lazy   as BS
import qualified Data.IntMap.Strict     as IM
import qualified Data.Map.Strict        as M
import           Data.Monoid
import           Data.Serialize         (Serialize (..), encode)
import           Options.Applicative
import           Pipes                  ((<-<))
import qualified Pipes                  as P
import qualified Pipes.ByteString       as PBS
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

  procmap <- processMapFromFiles (regex args) (P.each $ infiles args)

  let procmap' =
        IM.toList procmap <&>
          \(i, (d, m)) ->
            fmap ((i, d),)
            . concatMap sequenceA
            . M.toList
            . folderToMap
            . fmap (M.toList . variationsToMap "nominal")
            $ m

  BS.writeFile (outfile args)
    . compress
    . PBS.toLazy
    $ serializer
      <-< P.each procmap'

-- | Serialize data into strict ByteStrings.
serializer :: (Serialize a, Monad m) => P.Pipe a PBS.ByteString m ()
serializer = forever $ do
    x <- P.await
    P.yield (encode x)
