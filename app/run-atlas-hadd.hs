
module Main where

import           Atlas
import           Atlas.ToYoda
import           Codec.Compression.GZip (compress, decompress)
import           Control.DeepSeq
import qualified Control.Foldl          as F
import           Control.Lens
import qualified Data.ByteString.Lazy   as BS
import           Data.Monoid
import           Data.Serialize         (decodeLazy, encodeLazy)
import qualified List.Transformer       as L
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
  let f =
        F.FoldM
          (\x s -> merge x . toMaybe <$> decodeFile (regex args) s)
          (return Nothing)
          return

  out <-
    F.impurely L.foldM f
      (L.select (infiles args) :: L.ListT IO String)

  BS.writeFile (outfile args) (compress $ encodeLazy out)


  where
      merge x Nothing = x
      merge Nothing y = y
      merge (Just (dsid, sumwgt, hs)) (Just (dsid', sumwgt', hs')) =
          let x =
                if dsid == dsid'
                  then dsid
                  else error "attempt to add histograms with different dsids!"

              y = sumwgt + sumwgt'
              z = mappend hs hs'
          in z `deepseq` Just (x, y, z)

      toMaybe (Left _)  = Nothing
      toMaybe (Right x) = x
