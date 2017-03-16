{-# LANGUAGE TupleSections #-}

module Data.Atlas.PhysObj where

import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Maybe
import           Data.Atlas.Corrected
import           Data.Atlas.Variation

-- an MC object:
-- we have variations on the weights of the object
-- as well as variations on the object iself
-- when we run the whole thing, these Vars are combined.

type PhysObj = MaybeT (CorrectedT (Vars SF) Vars)
type POCont r = ContT r PhysObj

-- runPhysObj :: (a -> MaybePhysObj r a -> Vars (Maybe (a, Double))
runPhysObj
  :: POCont a a
  -- -> (a -> (MaybeT (CorrectedT (Vars SF) Vars)) r)
  -> PhysObj a
runPhysObj p = runContT p pure
