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

-- TODO
-- will switching the MaybeT and WriterT make things go faster??
-- the issue, I believe, is one of sharing.
-- currently, we only "force" the SFs to be calculated at the last second:
-- even after we've spread them out through the Vars dictionary...
-- we need a way to "force" the SFs without running MaybeT Vars...
-- the problem is that in all the monad transformers, the state/log/etc
-- live *inside* the underlying monad...
-- can we try just a tuple?!
type PhysObj = CorrectedT (Vars SF) (MaybeT Vars)

setWgt :: Monad m => Vars SF -> CorrectedT (Vars SF) m ()
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
