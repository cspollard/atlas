module Data.Atlas.Corrected
  ( CorrectedT, withCorrection, runCorrectedT, runCorrected
  , mapCorrectedT, Corrected, correctedT
  , ScaleFactor, SF, NoSF
  ) where

import           Control.Monad.Trans.Writer.Lazy
import           Data.Functor.Identity
import           Data.Monoid

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

withCorrection :: Monad m => (a, b) -> CorrectedT b m a
withCorrection = writer


mapCorrectedT
  :: (m1 (a1, w) -> m (a, b)) -> CorrectedT w m1 a1 -> CorrectedT b m a
mapCorrectedT = mapWriterT

type ScaleFactor = Product Double
type SF = ScaleFactor
type NoSF = ()
