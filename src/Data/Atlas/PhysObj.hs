{-# LANGUAGE TupleSections #-}

module Data.Atlas.PhysObj where

import           Control.Monad              (join)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Writer
import           Data.Atlas.Corrected
import           Data.Atlas.Variation
import           Data.Bifunctor             (second)
import           Data.Functor.Sum

-- an MC object:
-- we have variations on the weights of the object
-- as well as variations on the object iself
-- when we run the whole thing, these Vars are combined.
type MCObj = WriterT (Vars SF) (MaybeT Vars)

-- runMCObj :: MCObj a -> Vars (Maybe (Double, a))
runMCObj :: MCObj a -> Vars (Maybe (a, Double))
runMCObj =
  join
  -- Vars (Maybe (Vars (a, Double))
  . fmap (sequenceA . fmap sequenceA)
  -- Vars (Maybe (a, Vars Double))
  . runMaybeT
  -- MaybeT Vars (a, Vars Double)
  . runWriterT
  -- WriterT (Vars Double) (MaybeT Vars) a
  . mapWriterT (fmap (second (fmap runSF)))

type DataObj = Maybe

-- do we really need this to turn into vars?
runDataObj :: DataObj a -> Vars (Maybe (a, Double))
runDataObj = pure . fmap (,1)

type PhysObj = Sum DataObj MCObj

runPhysObj :: PhysObj a -> Vars (Maybe (a, Double))
runPhysObj (InL x) = runDataObj x
runPhysObj (InR x) = runMCObj x
