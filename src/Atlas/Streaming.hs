module Atlas.Streaming where

import qualified Data.ByteString.Lazy as BS
import Data.Serialize (encodeLazy, decodeLazy, Serialize)
import Control.Monad (foldM)


decodeFile :: Serialize a => String -> IO a
decodeFile f = either error return =<< (decodeLazy <$> BS.readFile f)


decodeFiles :: (Serialize a, Monoid a) => [String] -> IO a
decodeFiles = foldM (\x s -> (<> x) <$> decodeFile s) mempty


encodeFile :: Serialize a => String -> a -> IO ()
encodeFile s = BS.writeFile s . encodeLazy
