module Data.Atlas.Corrected
  ( CorrectedT, withCorrection, runCorrectedT, runCorrected
  , mapCorrectedT, Corrected, correctedT
  , ScaleFactor, SF, NoSF
  ) where

import           Control.Monad.Trans.Writer.Lazy
import           Data.Functor.Identity
import           Data.Monoid

-- a scale factor is just a writer monad with the underlying monoid (Reals, *)
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

-- instance Fractional a => Fractional (Product a) where
--   recip (Product x) = Product $ recip x
--   (Product x) / (Product y) = Product (x/y)
--
-- data Event =
--   Event
--     { mu   :: Corrected ScaleFactor Double
--     , jets :: ListT (Corrected ScaleFactor) Double
--     }
--
-- takeWhileLT :: Monad m => (a -> Bool) -> ListT m a -> ListT m a
-- takeWhileLT f l = ListT $ do
--   n <- next l
--   case n of
--     (Cons x l') ->
--       if f x
--         then return . Cons x $ takeWhileLT f l'
--         else return Nil
--     Nil -> return Nil
--
-- lengthLT :: Monad m => ListT m b -> m Int
-- lengthLT l = fold (+) 0 id $ const 1 <$> l
--
-- njets :: Event -> Corrected ScaleFactor Int
-- njets = lengthLT . takeWhileLT (> 30) . jets
