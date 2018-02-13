{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Atlas.PhysObj
  ( PhysObj(..), runPhysObj
  ) where

import           Atlas.ScaleFactor
import           Atlas.Variation
import           Control.Applicative
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer
import           GHC.Generics

-- TODO
-- go back to MaybeT (WriterT SF Vars) a?
-- which yields Vars (Maybe a, SF)
newtype PhysObj a = PhysObj { unPO :: MaybeT (WriterT SF Vars) a }
  deriving
    (Generic, Functor, Applicative, Monad, Alternative, MonadWriter SF, Show)


runPhysObj :: PhysObj a -> Vars (Maybe a, Double)
runPhysObj po = do
  (ma, s) <- runWriterT . runMaybeT $ unPO po
  return (ma, runSF s)



-- instance Show a => Show (PhysObj a) where
--   showsPrec n (PhysObj (ChronicleT v)) =
--     showString "PhysObj " . showParen True (showsPrec n v)


-- instance Foldable PhysObj where
--   foldMap f = foldMap (foldMap f) . runChronicleT . unPO


-- TODO
-- this is incredibly inelegant.
-- instance Crosswalk PhysObj where
--
--   crosswalk f (PhysObj ct) = PhysObj . ChronicleT <$> go ct'
--     where
--       ct' = runChronicleT $ f <$> ct
--
--       emptyVarMap :: VarMap (These SF a)
--       emptyVarMap = view variations $ const emptyThese <$> ct'
--
--       go :: forall f b. Align f => Vars (These SF (f b)) -> f (Vars (These SF b))
--       go (Variation x xs) =
--         let xs' :: f (VarMap (These SF b))
--             xs' =
--               foldrWithKey (\k y ys -> alignWith (g k) y ys) nil
--               $ sequenceL <$> xs
--             x' :: f (These SF b)
--             x' = sequenceL x
--         in alignWith h x' xs'
--
--       g :: T.Text -> These (These SF b) (VarMap (These SF b)) -> VarMap (These SF b)
--       g k (This x)     = singleton k x
--       g k (That xs)    = singleton k emptyThese `mappend` xs
--       g k (These x xs) = singleton k x `mappend` xs
--
--       h :: These (These SF b) (VarMap (These SF b)) -> Vars (These SF b)
--       h = uncurry Variation . fromThese emptyThese emptyVarMap
--
--
--       emptyThese :: Monoid a => These a b
--       emptyThese = This mempty
