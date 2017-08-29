{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module Atlas.PhysObj
  ( PhysObj(..), runPhysObj, runPhysObj', varSF, varObj, varsToPO
  , module X
  ) where

import           Atlas.ScaleFactor
import           Atlas.Variation
import           Control.Applicative
import           Control.Lens
import           Control.Monad.Chronicle as X
import           Data.Align              as X
import           Data.Key
import qualified Data.Text               as T
import           Data.These              as X
import           GHC.Generics

-- TODO
-- add ReaderT DataMC'?

-- TODO
-- go back to MaybeT (WriterT SF Vars) a?
-- which yields Vars (Maybe a, SF)
newtype PhysObj a = PhysObj { unPO :: ChronicleT SF Vars a }
  deriving
    (Generic, Functor, Applicative, Monad, Alternative, MonadChronicle SF)


instance Show a => Show (PhysObj a) where
  showsPrec n (PhysObj (ChronicleT v)) =
    showString "PhysObj " . showParen True (showsPrec n v)


instance Foldable PhysObj where
  foldMap f = foldMap (foldMap f) . runChronicleT . unPO


-- TODO
-- this is incredibly inelegant.
instance Crosswalk PhysObj where

  crosswalk f (PhysObj ct) = PhysObj . ChronicleT <$> go ct'
    where
      ct' = runChronicleT $ f <$> ct

      emptyVarMap :: VarMap (These SF a)
      emptyVarMap = view variations $ const emptyThese <$> ct'

      go :: forall f b. Align f => Vars (These SF (f b)) -> f (Vars (These SF b))
      go (Variation x xs) =
        let xs' :: f (VarMap (These SF b))
            xs' =
              foldrWithKey (\k y ys -> alignWith (g k) y ys) nil
              $ sequenceL <$> xs
            x' :: f (These SF b)
            x' = sequenceL x
        in alignWith h x' xs'

      g :: T.Text -> These (These SF b) (VarMap (These SF b)) -> VarMap (These SF b)
      g k (This x)     = singleton k x
      g k (That xs)    = singleton k emptyThese `mappend` xs
      g k (These x xs) = singleton k x `mappend` xs

      h :: These (These SF b) (VarMap (These SF b)) -> Vars (These SF b)
      h = uncurry Variation . fromThese emptyThese emptyVarMap


      emptyThese :: Monoid a => These a b
      emptyThese = This mempty




runPhysObj :: PhysObj a -> Vars (These Double a)
runPhysObj = fmap (mapThis runSF) . runPhysObj'


runPhysObj' :: PhysObj a -> Vars (These SF a)
runPhysObj' = runChronicleT . unPO


varSF :: Vars SF -> PhysObj ()
varSF sfs = PhysObj . ChronicleT $ flip These () <$> sfs

varObj :: Vars a -> PhysObj a
varObj xs = PhysObj . ChronicleT $ That <$> xs

varsToPO :: Vars (These SF a) -> PhysObj a
varsToPO = PhysObj . ChronicleT
