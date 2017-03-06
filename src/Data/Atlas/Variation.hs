{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}

module Data.Atlas.Variation where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Identity
import           Data.Functor.Identity

newtype VariationT (s :: k) m a = VariationT { unVary :: IdentityT m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

type Variation (s :: k) a = VariationT s Identity a

variationT :: m a -> VariationT s m a
variationT = VariationT . IdentityT

variation :: a -> Variation s a
variation = variationT . Identity

runVariationT :: VariationT s m a -> m a
runVariationT = runIdentityT . unVary

runVariation :: Variation s a -> a
runVariation = runIdentity . runVariationT
