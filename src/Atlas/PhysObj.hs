{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}

module Atlas.PhysObj where

import           Atlas.Corrected
import           Atlas.Variation
import           Control.DeepSeq
import           Control.Monad.Catch
import           Control.Monad.Morph
import           Control.Monad.Trans.Maybe
import           Data.Functor.Classes
import           Data.Functor.Identity
import           GHC.Generics

-- TODO
-- add ReaderT DataMC'

newtype PhysObjT m a = PhysObjT { unPOT :: WriterT SF (MaybeT (VarsT m)) a }
  deriving
    ( Generic, Functor, Applicative, Monad
    , MonadWriter SF, MonadIO, MonadThrow, MonadCatch, Show1, Show
    )

instance MonadTrans PhysObjT where
  lift = PhysObjT . lift . lift . lift

instance MFunctor PhysObjT where
  hoist f = PhysObjT . hoist (hoist $ hoist f) . unPOT

type PhysObj = PhysObjT Identity


runPhysObj :: Functor m => PhysObjT m a -> VarsT m (Maybe (a, Double))
runPhysObj = (fmap.fmap.fmap) runSF . runMaybeT . runWriterT . unPOT


varSF :: Functor m => VarsT m SF -> PhysObjT m ()
varSF sfs =
  -- MaybeT Vars (a, SF)
  PhysObjT . WriterT . MaybeT
  -- Vars (Maybe (a, SF))
  $ Just . ((),) <$> sfs

varObj :: Functor m => VarsT m a -> PhysObjT m a
varObj xs = PhysObjT . WriterT . MaybeT $ Just . (,mempty) <$> xs
