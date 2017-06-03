{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}

module Atlas.PhysObj where

import           Atlas.Corrected
import           Atlas.Variation
import qualified Control.Monad.Fail        as MF
import           Control.Monad.Trans.Maybe
import           Data.Functor.Classes
import           GHC.Generics

-- TODO
-- add ReaderT DataMC'

newtype PhysObj a = PhysObj { unPOT :: WriterT SF (MaybeT Vars) a }
  deriving (Generic, Functor, Applicative, Monad, MonadWriter SF, Show1, Show)


instance MF.MonadFail PhysObj where
  fail _ = PhysObj . WriterT . MaybeT $ return Nothing


runPhysObj :: PhysObj a -> Vars (Maybe (a, Double))
runPhysObj = (fmap.fmap.fmap) runSF . runMaybeT . runWriterT . unPOT

runPhysObj' :: PhysObj a -> Vars (Maybe (a, SF))
runPhysObj' = runMaybeT . runWriterT . unPOT


varSF :: Vars SF -> PhysObj ()
varSF sfs =
  PhysObj . WriterT . MaybeT
  $ Just . ((),) <$> sfs


varObj :: Vars a -> PhysObj a
varObj xs = PhysObj . WriterT . MaybeT $ Just . (,mempty) <$> xs
