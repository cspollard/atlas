{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

module Atlas.ScaleFactor
  (ScaleFactor, SF, runSF, sf
  ) where

import           Data.Hashable
import qualified Data.HashMap.Strict as HM
import           Data.Semigroup
import qualified Data.Text           as T

-- | ScaleFactor
-- | HashMap of `a`s indexed by `b`s
-- | with an accumulator of type `a` for efficiency
-- | (the constuctor is not exposed to prevent improper ScaleFactors)
-- | the semigroup instance merges the HashMaps and then folds over the map
-- | to update the accumulator. it is _left biased_.

data ScaleFactor b a = SF a (HM.HashMap b a)
  deriving (Show, Functor, Foldable, Traversable)

instance (Eq b, Hashable b, Semigroup a) => Semigroup (ScaleFactor b a) where
  SF x mx <> SF _ my =
    let dxy = my `HM.difference` mx
        mx' = dxy `HM.union` mx
        x' = foldl (<>) x dxy
    in SF x' mx'
  {-# INLINE (<>) #-}
  

instance (Eq b, Hashable b, Monoid a) => Monoid (ScaleFactor b a) where
  mempty = SF mempty mempty
  {-# INLINE mempty #-}

  SF x mx `mappend` SF _ my =
    let dxy = my `HM.difference` mx
        mx' = dxy `HM.union` mx
        x' = foldl mappend x dxy
    in SF x' mx'
  {-# INLINE mappend #-}


type SF = ScaleFactor T.Text (Product Double)

runSF :: ScaleFactor b (Product Double) -> Double
runSF (SF x _) = getProduct x
{-# INLINE runSF #-}

sf :: T.Text -> Double -> SF
sf t x = let x' = Product x in SF x' $ HM.singleton t x'
{-# INLINE sf #-}
