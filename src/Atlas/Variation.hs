{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Atlas.Variation
  ( module X
  , Vars
  , variationToMap, mapToVariation
  , StrictMap, strictMap, unSM, liftSM, liftSM2
  , inSM
  , lookup, intersectionWith, mapMaybeWithKey
  ) where

import           Control.Lens
import           Data.Data
import           Data.Functor.Bind.Class
import           Data.Functor.Classes
import qualified Data.Map.Strict         as M
import           Data.Semigroup
import           Data.Serialize
import           Data.SMonoid
import qualified Data.Text               as T
import           Data.Variation          as X
import           GHC.Exts
import           GHC.Generics
import           Prelude                 hiding (lookup)


type Vars = Variation (StrictMap T.Text)

variationToMap :: Ord k => k -> Variation (StrictMap k) a -> StrictMap k a
variationToMap k (Variation x xs) = xs & at k ?~ x

mapToVariation :: Ord k => k -> StrictMap k a -> Maybe (Variation (StrictMap k) a)
mapToVariation k m = do
  n <- m ^? ix k
  return . Variation n $ sans k m


newtype StrictMap k a = SM { unSM :: M.Map k a }
  deriving (Show, Eq, Ord, Data, Typeable, Generic, Show1)

strictMap :: M.Map k a -> StrictMap k a
strictMap = SM . force
  where
    force m = M.foldl' (flip seq) () m `seq` m

instance (Ord k, Serialize k, Serialize a) => Serialize (StrictMap k a) where

inSM :: (M.Map t1 t2 -> t) -> StrictMap t1 t2 -> t
inSM f (SM m) = f m

liftSM :: (M.Map k1 a1 -> M.Map k a) -> StrictMap k1 a1 -> StrictMap k a
liftSM f = strictMap . f . unSM

liftSM2
  :: (M.Map t2 t3 -> M.Map t t1 -> M.Map k a)
  -> StrictMap t2 t3
  -> StrictMap t t1
  -> StrictMap k a
liftSM2 f (SM sm) (SM sm') = strictMap $ f sm sm'

lookup :: Ord k => k -> StrictMap k a -> Maybe a
lookup k = inSM (M.lookup k)

mapMaybeWithKey :: (k -> a1 -> Maybe a) -> StrictMap k a1 -> StrictMap k a
mapMaybeWithKey f (SM m) = SM $ M.mapMaybeWithKey f m

intersectionWith
  :: Ord k
  => (a1 -> b -> a) -> StrictMap k a1 -> StrictMap k b -> StrictMap k a
intersectionWith f (SM m) (SM m') = SM $ M.intersectionWith f m m'

instance Functor (StrictMap k) where
  fmap f (SM m) = SM $ M.map f m

instance Foldable (StrictMap k) where
  foldMap f (SM m) = M.foldMapWithKey (const f) m

instance Traversable (StrictMap k) where
  traverse f (SM m) = SM <$> M.traverseWithKey (const f) m

instance Ord k => Semigroup (StrictMap k a) where
  SM m <> SM m' = SM $ M.union m m'

instance Ord k => Monoid (StrictMap k a) where
  mempty = SM M.empty
  mappend = (<>)

instance SUnit (StrictMap k) where
  sempty = SM M.empty

instance Ord k => SAppend (StrictMap k) where
  sappend = (<>)

instance Ord k => SMonoid (StrictMap k) where

type instance Index (StrictMap k a) = k
type instance IxValue (StrictMap k a) = a

instance Ord k => At (StrictMap k a) where
  at k f (SM m) =
    f mv <&> \r -> case r of
      Nothing -> maybe (SM m) (const (SM $ M.delete k m)) mv
      Just v' -> SM $ M.insert k v' m
    where mv = M.lookup k m

instance Ord k => Ixed (StrictMap k a) where
  ix k f (SM m) =
    case M.lookup k m of
     Just v  -> f v <&> \v' -> SM (M.insert k v' m)
     Nothing -> pure $ SM m

instance Ord k => Apply (StrictMap k) where
  (<.>) = intersectionWith id
  (<. ) = intersectionWith const
  ( .>) = intersectionWith (const id)

instance Ord k => Bind (StrictMap k) where
  m >>- f = mapMaybeWithKey (\k -> lookup k . f) m

instance Ord k => IsList (StrictMap k a) where
  type Item (StrictMap k a) = (k, a)
  fromList = SM . fromList
  toList = toList . unSM
