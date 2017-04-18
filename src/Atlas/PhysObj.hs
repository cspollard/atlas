{-# LANGUAGE TupleSections #-}

module Atlas.PhysObj where

import           Atlas.Corrected
import           Atlas.Variation
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer
import           Data.Tuple                (swap)

-- TODO
-- add ReaderT DataMC'

-- an MC object:
-- we have variations on the weights of the object
-- as well as variations on the object iself
-- these variations must be combined *by the function using them*
-- otherwise we run into severe performance issues

-- TODO
-- should this be swapped?
-- does this work if SFs are dependent on the contained value?
-- TODO
-- can we move Vars SF all the way inside so we only run them once?
-- MaybeT (VarsT (CorrectedT (Vars SF)))
type PhysObj = MaybeT (CorrectedT (Vars SF) Vars)

onlyObjVars :: Vars a -> PhysObj a
onlyObjVars = lift . lift
{-# INLINABLE onlyObjVars #-}

onlySFVars :: Vars SF -> a -> PhysObj a
onlySFVars sfs x = tell sfs >> return x
{-# INLINABLE onlySFVars #-}

runPhysObj :: PhysObj a -> Vars (Maybe (a, Double))
runPhysObj =
  fmap (fmap swap . sequenceA . swap)
  -- Vars (Maybe a, Double)
  . join
  -- Vars (Vars (Maybe a, Double))
  . fmap (sequenceA . (fmap.fmap) runSF)
  -- Vars (Maybe a, Vars SF)
  . runWriterT
  -- CorrectedT (Vars SF) Vars (Maybe a)
  . runMaybeT
  -- PhysObj a
{-# INLINABLE runPhysObj #-}
