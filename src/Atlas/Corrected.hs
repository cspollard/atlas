module Atlas.Corrected
  ( CorrectedT, withCorrection, runCorrectedT, runCorrected
  , mapCorrectedT, Corrected, correctedT
  , ScaleFactor, SF, runSF, sf
  ) where

import           Control.Monad.Writer.Lazy hiding ((<>))
import           Data.Functor.Identity
import qualified Data.Map                  as M
import           Data.Semigroup
import qualified Data.Text                 as T

-- a scale factor is just a writer monad with the underlying monoid of
-- Reals under multiplication.
type CorrectedT = WriterT
type Corrected b = CorrectedT b Identity

correctedT :: m (a, b) -> CorrectedT b m a
correctedT = WriterT

runCorrectedT :: CorrectedT w m a -> m (a, w)
runCorrectedT = runWriterT

runCorrected :: Corrected w a -> (a, w)
runCorrected = runIdentity . runCorrectedT

withCorrection :: (Monad m, Monoid b) => (a, b) -> CorrectedT b m a
withCorrection = writer


mapCorrectedT
  :: (m1 (a1, w) -> m (a, b)) -> CorrectedT w m1 a1 -> CorrectedT b m a
mapCorrectedT = mapWriterT


data ScaleFactor a = SF a (M.Map T.Text a) deriving (Show)

instance Semigroup a => Semigroup (ScaleFactor a) where
  SF x mx <> SF _ my =
    let dxy = my `M.difference` mx
        mx' = dxy `M.union` mx
        x' = foldl (<>) x dxy
    in SF x' mx'

instance Monoid a => Monoid (ScaleFactor a) where
  mempty = SF mempty mempty
  {-# INLINABLE mempty #-}

  a `mappend` b = unwrapMonoid $ WrapMonoid a <> WrapMonoid b
  {-# INLINABLE mappend #-}


type SF = ScaleFactor (Product Double)

runSF :: SF -> Double
runSF (SF x _) = getProduct x
{-# INLINABLE runSF #-}

sf :: T.Text -> Double -> SF
sf t x = let x' = Product x in SF x' $ M.singleton t x'
{-# INLINABLE sf #-}
