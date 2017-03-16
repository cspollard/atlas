{-# LANGUAGE TupleSections #-}

module Data.Atlas.PhysObj where

import           Control.Monad              (join)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Writer
import           Data.Atlas.Corrected
import           Data.Atlas.Variation
import           Data.Bifunctor             (second)

-- an MC object:
-- we have variations on the weights of the object
-- as well as variations on the object iself
-- when we run the whole thing, these Vars are combined.
type PhysObj = WriterT (Vars SF) (MaybeT Vars)

setWgt :: Monad m => Vars SF -> WriterT (Vars SF) m ()
setWgt = tell

runPhysObj :: PhysObj a -> Vars (Maybe (a, Double))
runPhysObj =
  join
  -- Vars (Maybe (Vars (a, Double))
  . fmap (sequenceA . fmap sequenceA)
  -- Vars (Maybe (a, Vars Double))
  . runMaybeT
  -- MaybeT Vars (a, Vars Double)
  . runWriterT
  -- WriterT (Vars Double) (MaybeT Vars) a
  . mapWriterT (fmap (second (fmap runSF)))
