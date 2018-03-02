{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module Atlas.PhysObj
  ( PhysObj(..), runPhysObj, collapsePO, poFromVars, varSF, poFail
  ) where

import           Atlas.ScaleFactor
import           Atlas.Variation
import           Control.Applicative
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer.Strict
import           Data.Align
import           Data.Key
import           Data.These
import           GHC.Generics


newtype PhysObj a = PhysObj { unPO :: MaybeT (WriterT SF Vars) a }
  deriving
    (Generic, Functor, Applicative, Monad, Alternative, MonadWriter SF)


runPhysObj :: PhysObj a -> Vars (Maybe a, SF)
runPhysObj = runWriterT . runMaybeT . unPO


instance Show a => Show (PhysObj a) where
  show (PhysObj (MaybeT (WriterT v))) =
    "PhysObj (" ++ show v ++ ")"


poFromVars :: Vars a -> PhysObj a
poFromVars = PhysObj . MaybeT . WriterT . fmap ((,mempty) . Just)

poFail :: PhysObj a
poFail = PhysObj . MaybeT . WriterT $ Variation (Nothing, mempty) mempty

varSF :: Vars SF -> PhysObj ()
varSF = PhysObj . MaybeT . WriterT . fmap (Just (),)


collapsePO :: PhysObj [a] -> [PhysObj a]
collapsePO po =
  let vs' = runPhysObj po
      Variation n v = toTup <$> vs'
      vars = foldrWithKey goFold mempty v
      vars' = alignWith (theseToVar $ (Nothing, mempty) <$ v) n vars
  in PhysObj . MaybeT . WriterT <$> vars'

  where
    goFold k = alignWith (goThese k)

    goThese k (This (x, s))     = singleton k (Just x, s)
    -- TODO
    -- dropping inefficiency SFs
    goThese k (That xs)         = singleton k (Nothing, mempty) `mappend` xs
    goThese k (These (x, s) xs) = singleton k (Just x, s) `mappend` xs

    toTup (Just fxs, s) = (,s) <$> fxs
    toTup (Nothing, _)  = []

    theseToVar vm (This (n, s))   = Variation (Just n, s) vm
    theseToVar _ (That v)         = Variation (Nothing, mempty) v
    theseToVar _ (These (n, s) v) = Variation (Just n, s) v

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
