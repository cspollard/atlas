{-# LANGUAGE TupleSections #-}

module Data.Atlas.PhysObj where

import           Control.Monad             (join)
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Maybe
import           Data.Atlas.Corrected
import           Data.Atlas.Variation
import           Data.Bifunctor            (second)

-- an MC object:
-- we have variations on the weights of the object
-- as well as variations on the object iself
-- when we run the whole thing, these Vars are combined.

type PhysObj r = ContT r (MaybeT (CorrectedT (Vars SF) Vars))

-- runPhysObj :: (a -> MaybePhysObj r a -> Vars (Maybe (a, Double))
runPhysObj
  :: PhysObj a a
  -- -> (a -> (MaybeT (CorrectedT (Vars SF) Vars)) r)
  -> (MaybeT (CorrectedT (Vars SF) Vars)) a
runPhysObj p = runContT p pure
