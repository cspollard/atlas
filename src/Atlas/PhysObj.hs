{-# LANGUAGE TupleSections #-}

module Atlas.PhysObj where

import           Atlas.Corrected
import           Atlas.Variation
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Writer

-- TODO
-- add ReaderT DataMC'

-- an MC object:
-- we have variations on the weights of the object
-- as well as variations on the object iself
-- these variations must be combined *by the function using them*
-- otherwise we run into severe performance issues
type PhysObj = CorrectedT (Vars SF) (MaybeT Vars)

onlyObjVars :: Vars a -> PhysObj a
onlyObjVars = lift.lift
{-# INLINABLE onlyObjVars #-}

onlySFVars :: Monad m => Vars SF -> a -> CorrectedT (Vars SF) m a
onlySFVars sfs x = tell sfs >> return x
{-# INLINABLE onlySFVars #-}

runPhysObj :: PhysObj a -> Vars (Maybe (a, Vars SF))
runPhysObj = runMaybeT . runWriterT
{-# INLINABLE runPhysObj #-}
