{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Atlas.Variation
  ( module X
  , Vars, copyVars
  , variationToMap, mapToVariation
  ) where

import           Control.Lens
import qualified Data.StrictHashMap as SHM
import Data.Hashable
import qualified Data.Text            as T
import           Data.Variation       as X
import GHC.Exts (fromList)


type Vars = Variation (SHM.StrictHashMap T.Text)

instance Traversable f => Traversable (Variation f) where
  traverse f (Variation a as) = Variation <$> f a <*> traverse f as

instance Foldable f => Foldable (Variation f) where
  foldMap f (Variation a as) = f a <> foldMap f as

copyVars :: [T.Text] -> a -> Vars a
copyVars ts a = Variation a . fromList $ (, a) <$> ts


variationToMap
  :: (IxValue (t a) ~ a, At (t a))
  => Index (t a) -> Variation t a -> t a
variationToMap k (Variation x xs) = xs & at k ?~ x


mapToVariation
  :: (Eq k, Ord k, Hashable k)
  => k -> SHM.StrictHashMap k a -> Maybe (Variation (SHM.StrictHashMap k) a)
mapToVariation k m = do
  n <- m ^? ix k
  return . Variation n $ sans k m
