{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Atlas.StrictHashMap
  ( module X
  , StrictHashMap, strictHashMap, unSM, liftSM, liftSM2
  , singleton, inSM, smFromList
  , lookup, intersectionWith, mapMaybeWithKey
  ) where

import           Control.Lens
import           Data.Align
import           Data.Data
import           Data.Functor.Classes
import           Data.Hashable
import qualified Data.HashMap.Strict  as HM
import           Data.Key
import           Data.Semigroup
import           Data.Serialize
import           Data.Variation       as X
import           GHC.Exts             (IsList (..))
import           GHC.Generics
import           Linear.Matrix        (Trace (..))
import           Prelude              hiding (lookup)


newtype StrictHashMap k a = SM { unSM :: HM.HashMap k a }
  deriving (Show, Eq, Data, Typeable, Generic)

makePrisms ''StrictHashMap


strictHashMap :: HM.HashMap k a -> StrictHashMap k a
strictHashMap = SM . force


force :: HM.HashMap k a -> HM.HashMap k a
force m = HM.foldl' (flip seq) () m `seq` m


singleton :: Hashable k => k -> a -> StrictHashMap k a
singleton k = SM . HM.singleton k


instance (Ord k, Hashable k, Serialize k, Serialize a)
  => Serialize (StrictHashMap k a) where
  put = put . HM.toList . unSM
  get = strictHashMap . HM.fromList <$> get


smFromList :: (Hashable k, Ord k) => [(k, a)] -> StrictHashMap k a
smFromList = fromList


inSM :: (HM.HashMap t1 t2 -> t) -> StrictHashMap t1 t2 -> t
inSM f (SM m) = f m


liftSM :: (HM.HashMap k1 a1 -> HM.HashMap k a) -> StrictHashMap k1 a1 -> StrictHashMap k a
liftSM f = strictHashMap . f . unSM


liftSM2
  :: (HM.HashMap t2 t3 -> HM.HashMap t t1 -> HM.HashMap k a)
  -> StrictHashMap t2 t3
  -> StrictHashMap t t1
  -> StrictHashMap k a
liftSM2 f (SM sm) (SM sm') = strictHashMap $ f sm sm'


type instance Key (StrictHashMap k) = k


instance Keyed (StrictHashMap k) where
  mapWithKey f (SM m) = SM $ HM.mapWithKey f m


instance FoldableWithKey (StrictHashMap k) where
  foldMapWithKey f = foldMapWithKey f . unSM


mapMaybeWithKey :: (k -> a1 -> Maybe a) -> StrictHashMap k a1 -> StrictHashMap k a
mapMaybeWithKey f (SM m) = SM $ HM.mapMaybeWithKey f m


intersectionWith
  :: (Hashable k, Ord k)
  => (a1 -> b -> a) -> StrictHashMap k a1 -> StrictHashMap k b -> StrictHashMap k a
intersectionWith f (SM m) (SM m') = SM $ HM.intersectionWith f m m'

instance Functor (StrictHashMap k) where
  fmap f (SM m) = SM $ HM.map f m

instance Foldable (StrictHashMap k) where
  foldr f start = foldr f start . unSM

instance Traversable (StrictHashMap k) where
  traverse f (SM m) = SM <$> HM.traverseWithKey (const f) m

instance (Hashable k, Ord k) => Align (StrictHashMap k) where
  nil = mempty
  align (SM m) (SM m') = strictHashMap $ align m m'

instance (Hashable k, Ord k) => Trace (StrictHashMap k) where
  diagonal (SM m) = SM . force . diagonal $ unSM <$> m

instance (Hashable k, Ord k) => Semigroup (StrictHashMap k a) where
  SM m <> SM m' = SM $ HM.union m m'

instance (Hashable k, Ord k) => Monoid (StrictHashMap k a) where
  mempty = SM HM.empty
  mappend = (<>)

type instance Index (StrictHashMap k a) = k
type instance IxValue (StrictHashMap k a) = a


instance (Hashable k, Ord k) => FunctorWithIndex k (StrictHashMap k) where
  imap f = over _SM (HM.mapWithKey f)

instance (Hashable k, Ord k) => FoldableWithIndex k (StrictHashMap k) where
  ifoldMap f (SM m) = ifoldMap f m

instance (Hashable k, Ord k) => TraversableWithIndex k (StrictHashMap k) where
  itraverse f (SM m) = SM <$> itraverse f m

instance (Hashable k, Ord k) => At (StrictHashMap k a) where
  at k f (SM m) =
    f mv <&> \r -> case r of
      Nothing -> maybe (SM m) (const (SM $ HM.delete k m)) mv
      Just v' -> SM $ HM.insert k v' m
    where mv = HM.lookup k m


instance (Hashable k, Ord k) => Ixed (StrictHashMap k a) where
  ix k f (SM m) =
    case HM.lookup k m of
     Just v  -> f v <&> \v' -> SM (HM.insert k v' m)
     Nothing -> pure $ SM m


instance (Hashable k, Ord k) => IsList (StrictHashMap k a) where
  type Item (StrictHashMap k a) = (k, a)
  fromList = strictHashMap . fromList
  toList = toList . unSM


instance Show2 StrictHashMap where
  liftShowsPrec2 spk slk spv slv d (SM m) =
    showsUnaryWith (liftShowsPrec sp sl) "fromList" d (HM.toList m)
    where
      sp = liftShowsPrec2 spk slk spv slv
      sl = liftShowList2 spk slk spv slv

instance Show k => Show1 (StrictHashMap k) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
