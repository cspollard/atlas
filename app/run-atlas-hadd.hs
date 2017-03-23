
module Main where

import           Atlas.Variation
import           Codec.Compression.GZip (compress, decompress)
import           Control.DeepSeq
import qualified Control.Foldl          as F
import qualified Data.ByteString.Lazy   as BS
import           Data.Monoid
import           Data.Serialize         (decodeLazy, encodeLazy)
import           Data.YODA.Obj
import qualified List.Transformer       as L
import           Options.Applicative

data InArgs =
  InArgs
    { outfile :: String
    , infiles :: [String]
    }

inArgs :: Parser InArgs
inArgs =
  InArgs
    <$> strOption
      ( long "outfile"
      <> short 'o'
      <> metavar "OUTFILE" )
    <*> some (strArgument (metavar "INFILES"))

main :: IO ()
main = do
  args <- execParser $ info (helper <*> inArgs) fullDesc
  let f =
        F.FoldM
          (\x s -> merge x . toMaybe <$> decodeFile s)
          (return Nothing)
          return

  out <-
    F.impurely L.foldM f
      (L.select (infiles args) :: L.ListT IO String)

  BS.writeFile (outfile args) (compress . encodeLazy $ out)


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
          in z `seq` Just (x, y, z)

      toMaybe (Left _)  = Nothing
      toMaybe (Right x) = x


decodeFile
  :: String -> IO (Either String (Maybe (Int, Double, Folder (Vars YodaObj))))
decodeFile f = do
  putStrLn ("decoding file " ++ f)
  -- make sure we actually read the entire structure in.
  force . decodeLazy . decompress <$> BS.readFile f
