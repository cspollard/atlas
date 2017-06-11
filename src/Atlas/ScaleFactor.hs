module Atlas.ScaleFactor
  (ScaleFactor, SF, runSF, sf
  ) where

import qualified Data.HashMap.Strict as HM
import           Data.Semigroup
import qualified Data.Text           as T

data ScaleFactor a = SF !a !(HM.HashMap T.Text a) deriving Show

instance Semigroup a => Semigroup (ScaleFactor a) where
  SF x mx <> SF _ my =
    let dxy = my `HM.difference` mx
        mx' = dxy `HM.union` mx
        x' = foldl (<>) x dxy
    in SF x' mx'

instance Monoid a => Monoid (ScaleFactor a) where
  mempty = SF mempty mempty
  {-# INLINABLE mempty #-}

  -- TODO
  -- why does this make a difference?
  -- a `mappend` b = unwrapMonoid $ WrapMonoid a <> WrapMonoid b
  SF x mx `mappend` SF _ my =
    let dxy = my `HM.difference` mx
        mx' = dxy `HM.union` mx
        x' = foldl mappend x dxy
    in SF x' mx'
  {-# INLINABLE mappend #-}


type SF = ScaleFactor (Product Double)

runSF :: SF -> Double
runSF (SF x _) = getProduct x
{-# INLINABLE runSF #-}

sf :: T.Text -> Double -> SF
sf t x = let x' = Product x in SF x' $ HM.singleton t x'
{-# INLINABLE sf #-}
