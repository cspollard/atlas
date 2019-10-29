{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Atlas.StrictHashMap
  ( module X
  , StrictHashMap, strictHashMap, unSHM, liftSHM, liftSHM2
  , singleton, inSHM, shmFromList
  , lookup, intersectionWith, mapMaybeWithKey
  ) where

import           Control.Lens
import           Data.Align
import           Data.Data
import           Data.Functor.Classes
import           Data.Hashable
import qualified Data.HashMap.Strict  as HM
import           Data.Key hiding (zip)
import           Data.Semigroup
import           Data.Serialize
import           Data.Variation       as X
import           GHC.Exts             (IsList (..))
import           GHC.Generics
import           Linear.Matrix        (Trace (..))
import           Prelude              hiding (lookup, zip)


newtype StrictHashMap k a = SHM { unSHM :: HM.HashMap k a }
  deriving (Show, Eq, Data, Typeable, Generic)

makePrisms ''StrictHashMap


strictHashMap :: HM.HashMap k a -> StrictHashMap k a
strictHashMap = SHM . force


force :: HM.HashMap k a -> HM.HashMap k a
force m = HM.foldl' (flip seq) () m `seq` m


singleton :: Hashable k => k -> a -> StrictHashMap k a
singleton k = SHM . HM.singleton k


instance (Ord k, Hashable k, Serialize k, Serialize a)
  => Serialize (StrictHashMap k a) where
  put = put . HM.toList . unSHM
  get = strictHashMap . HM.fromList <$> get


shmFromList :: (Hashable k, Ord k) => [(k, a)] -> StrictHashMap k a
shmFromList = fromList


inSHM :: (HM.HashMap t1 t2 -> t) -> StrictHashMap t1 t2 -> t
inSHM f (SHM m) = f m


liftSHM :: (HM.HashMap k1 a1 -> HM.HashMap k a) -> StrictHashMap k1 a1 -> StrictHashMap k a
liftSHM f = strictHashMap . f . unSHM


liftSHM2
  :: (HM.HashMap t2 t3 -> HM.HashMap t t1 -> HM.HashMap k a)
  -> StrictHashMap t2 t3
  -> StrictHashMap t t1
  -> StrictHashMap k a
liftSHM2 f (SHM sm) (SHM sm') = strictHashMap $ f sm sm'


type instance Key (StrictHashMap k) = k


instance Keyed (StrictHashMap k) where
  mapWithKey f (SHM m) = SHM $ HM.mapWithKey f m


instance FoldableWithKey (StrictHashMap k) where
  foldMapWithKey f = foldMapWithKey f . unSHM


mapMaybeWithKey :: (k -> a1 -> Maybe a) -> StrictHashMap k a1 -> StrictHashMap k a
mapMaybeWithKey f (SHM m) = SHM $ HM.mapMaybeWithKey f m


intersectionWith
  :: (Hashable k, Ord k)
  => (a1 -> b -> a) -> StrictHashMap k a1 -> StrictHashMap k b -> StrictHashMap k a
intersectionWith f (SHM m) (SHM m') = SHM $ HM.intersectionWith f m m'

instance Functor (StrictHashMap k) where
  fmap f (SHM m) = SHM $ HM.map f m

instance Foldable (StrictHashMap k) where
  foldr f start = foldr f start . unSHM

instance Traversable (StrictHashMap k) where
  traverse f (SHM m) = SHM <$> HM.traverseWithKey (const f) m

instance (Hashable k, Ord k) => Semialign (StrictHashMap k) where
  align (SHM m) (SHM m') = strictHashMap $ align m m'
  zip (SHM m) (SHM m') = strictHashMap $ zip m m'

instance (Hashable k, Ord k) => Align (StrictHashMap k) where
  nil = mempty

instance (Hashable k, Ord k) => Trace (StrictHashMap k) where
  diagonal (SHM m) = SHM . force . diagonal $ unSHM <$> m

instance (Hashable k, Ord k) => Semigroup (StrictHashMap k a) where
  SHM m <> SHM m' = SHM $ HM.union m m'

instance (Hashable k, Ord k) => Monoid (StrictHashMap k a) where
  mempty = SHM HM.empty
  mappend = (<>)

type instance Index (StrictHashMap k a) = k
type instance IxValue (StrictHashMap k a) = a


instance (Hashable k, Ord k) => FunctorWithIndex k (StrictHashMap k) where
  imap f = over _SHM (HM.mapWithKey f)

instance (Hashable k, Ord k) => FoldableWithIndex k (StrictHashMap k) where
  ifoldMap f (SHM m) = ifoldMap f m

instance (Hashable k, Ord k) => TraversableWithIndex k (StrictHashMap k) where
  itraverse f (SHM m) = SHM <$> itraverse f m

instance (Hashable k, Ord k) => At (StrictHashMap k a) where
  at k f (SHM m) =
    f mv <&> \r -> case r of
      Nothing -> maybe (SHM m) (const (SHM $ HM.delete k m)) mv
      Just v' -> SHM $ HM.insert k v' m
    where mv = HM.lookup k m


instance (Hashable k, Ord k) => Ixed (StrictHashMap k a) where
  ix k f (SHM m) =
    case HM.lookup k m of
     Just v  -> f v <&> \v' -> SHM (HM.insert k v' m)
     Nothing -> pure $ SHM m


instance (Hashable k, Ord k) => IsList (StrictHashMap k a) where
  type Item (StrictHashMap k a) = (k, a)
  fromList = strictHashMap . fromList
  toList = toList . unSHM


instance Show2 StrictHashMap where
  liftShowsPrec2 spk slk spv slv d (SHM m) =
    showsUnaryWith (liftShowsPrec sp sl) "fromList" d (HM.toList m)
    where
      sp = liftShowsPrec2 spk slk spv slv
      sl = liftShowList2 spk slk spv slv

instance Show k => Show1 (StrictHashMap k) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
