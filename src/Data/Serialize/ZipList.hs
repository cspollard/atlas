module Data.Serialize.ZipList where

import Data.Serialize
import Control.Applicative (ZipList(..))

instance Serialize a => Serialize (ZipList a) where
    put = put . getZipList
    get = ZipList <$> get
