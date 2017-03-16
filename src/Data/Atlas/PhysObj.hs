{-# LANGUAGE TupleSections #-}

module Data.Atlas.PhysObj where

import           Control.Monad              (join)
import           Control.Monad.Trans.Writer
import           Data.Atlas.Corrected
import           Data.Atlas.Variation
import           Data.Functor.Identity
import           Data.Functor.Sum
import           Data.Tuple                 (swap)

-- an MC object:
-- we have variations on the weights of the object
-- as well as variations on the object iself
-- when we run the whole thing, these Vars are combined.
type MCObj = WriterT (Vars SF) Vars

runMCObj :: MCObj a -> Vars (Double, a)
runMCObj = fmap swap . join . fmap ((fmap.fmap) runSF . sequenceA) . runWriterT

type DataObj = Identity

-- do we really need this to turn into vars?
runDataObj :: DataObj a -> Vars (Double, a)
runDataObj = pure . (1,) . runIdentity

type PhysObj = Sum DataObj MCObj

runPhysObj :: PhysObj a -> Vars (Double, a)
runPhysObj (InL x) = runDataObj x
runPhysObj (InR x) = runMCObj x
