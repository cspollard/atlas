{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}

module Data.Atlas.Variation where

import           Control.Monad.Trans.Identity
import           Data.Functor.Identity
import           Data.Kind

type VariationT (s :: k) = IdentityT
-- newtype VariationT (s :: k) m a = VariationT { unVary :: IdentityT m a }
--   deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

type Variation (s :: k) = VariationT s Identity

variationT :: m a -> VariationT s m a
variationT = IdentityT

mapVariationT
  :: forall k k1 (m :: k1 -> *) (a :: k1) (n :: k -> *) (b :: k).
  (m a -> n b) -> IdentityT m a -> IdentityT n b
mapVariationT = mapIdentityT

variation :: a -> Variation s a
variation = variationT . Identity

runVariationT :: VariationT s m a -> m a
runVariationT = runIdentityT

runVariation :: Variation s a -> a
runVariation = runIdentity . runVariationT
