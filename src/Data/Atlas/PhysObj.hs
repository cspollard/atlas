{-# LANGUAGE TupleSections #-}

module Data.Atlas.PhysObj where

import           Control.Monad.Trans.Maybe
import           Data.Atlas.Corrected
import           Data.Atlas.Variation

-- an MC object:
-- we have variations on the weights of the object
-- as well as variations on the object iself
-- these variations must be combined *by the function using them*
-- otherwise we run into severe performance issues
type PhysObj = CorrectedT (Vars SF) (MaybeT Vars)

setWgt :: Monad m => Vars SF -> CorrectedT (Vars SF) m ()
setWgt = tell

runPhysObj :: PhysObj a -> Vars (Maybe (a, Vars SF))
runPhysObj = runMaybeT . runWriterT
{-# INLINABLE runPhysObj #-}
