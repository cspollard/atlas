module Data.Serialize.CTypes where

import Foreign.C.Types
import Data.Serialize

instance Serialize CChar where
    put (CChar x) = put x
    get = CChar <$> get

instance Serialize CInt where
    put (CInt x) = put x
    get = CInt <$> get

instance Serialize CLong where
    put (CLong x) = put x
    get = CLong <$> get
