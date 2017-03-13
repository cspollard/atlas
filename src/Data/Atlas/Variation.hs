{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeInType                 #-}

module Data.Atlas.Variation
  ( VariationT, Variation, variationT, variation
  , mapVariationT, mapVariation
  , runVariationT, runVariation
  , NominalT, Nominal
  ) where

import           Control.Monad.Trans.Class
import           Control.Monad.IO.Class
import           Data.Functor.Identity
import           Data.Proxy
import           Data.Text                 as T
import           GHC.TypeLits

-- TODO
-- I'm not sure why I need a newtype here instead of a type synonym.
-- this is exactly a tagged IdentityT...
newtype VariationT (s :: k) m a = VT { unVT :: m a }
  deriving (Functor, Applicative, Monad, Foldable, Traversable, Show, Monoid)

type Variation (s :: k) = VariationT s Identity
type NominalT = VariationT "nominal"
type Nominal = NominalT Identity

instance MonadTrans (VariationT s) where
  lift = VT

instance MonadIO m => MonadIO (VariationT s m) where
  liftIO = lift . liftIO

variationT :: m a -> VariationT s m a
variationT = VT

variation :: a -> Variation s a
variation = variationT . Identity

mapVariationT :: (m a -> n b) -> VariationT s m a -> VariationT s n b
mapVariationT f = VT . f . unVT

mapVariation :: (a -> b) -> Variation s a -> Variation s b
mapVariation = mapVariationT . fmap

runVariationT :: forall s f t. KnownSymbol s => VariationT s f t -> (Text, f t)
runVariationT vsft =
  (T.pack $ symbolVal (Proxy :: Proxy s), unVT vsft)

runVariation :: KnownSymbol s => Variation s a -> (Text, a)
runVariation = fmap runIdentity . runVariationT
