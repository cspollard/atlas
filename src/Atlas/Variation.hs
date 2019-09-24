{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Atlas.Variation
  ( module X
  , Vars, VarMap, copyVars
  , variationToMap, mapToVariation
  , StrictMap, strictMap, unSM, liftSM, liftSM2
  , singleton, inSM
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
import qualified Data.Text            as T
import           Data.Variation       as X
import           GHC.Exts             (IsList (..))
import           GHC.Generics
import           Linear.Matrix        (Trace (..))
import           Prelude              hiding (lookup)


type VarMap = StrictMap T.Text
type Vars = Variation VarMap


copyVars :: [T.Text] -> a -> Vars a
copyVars ts a = Variation a . SM . HM.fromList $ (, a) <$> ts


newtype StrictMap k a = SM { unSM :: HM.HashMap k a }
  deriving (Show, Eq, Data, Typeable, Generic)

makePrisms ''StrictMap


variationToMap
  :: (IxValue (t a) ~ a, At (t a))
  => Index (t a) -> Variation t a -> t a
variationToMap k (Variation x xs) = xs & at k ?~ x


mapToVariation
  :: (Eq k, Ord k, Hashable k)
  => k -> StrictMap k a -> Maybe (Variation (StrictMap k) a)
mapToVariation k m = do
  n <- m ^? ix k
  return . Variation n $ sans k m


strictMap :: HM.HashMap k a -> StrictMap k a
strictMap = SM . force


force :: HM.HashMap k a -> HM.HashMap k a
force m = HM.foldl' (flip seq) () m `seq` m


singleton :: Hashable k => k -> a -> StrictMap k a
singleton k = SM . HM.singleton k


instance (Ord k, Hashable k, Serialize k, Serialize a)
  => Serialize (StrictMap k a) where
  put = put . HM.toList . unSM
  get = strictMap . HM.fromList <$> get


inSM :: (HM.HashMap t1 t2 -> t) -> StrictMap t1 t2 -> t
inSM f (SM m) = f m


liftSM :: (HM.HashMap k1 a1 -> HM.HashMap k a) -> StrictMap k1 a1 -> StrictMap k a
liftSM f = strictMap . f . unSM


liftSM2
  :: (HM.HashMap t2 t3 -> HM.HashMap t t1 -> HM.HashMap k a)
  -> StrictMap t2 t3
  -> StrictMap t t1
  -> StrictMap k a
liftSM2 f (SM sm) (SM sm') = strictMap $ f sm sm'


type instance Key (StrictMap k) = k


instance Keyed (StrictMap k) where
  mapWithKey f (SM m) = SM $ HM.mapWithKey f m


instance FoldableWithKey (StrictMap k) where
  foldMapWithKey f = foldMapWithKey f . unSM


mapMaybeWithKey :: (k -> a1 -> Maybe a) -> StrictMap k a1 -> StrictMap k a
mapMaybeWithKey f (SM m) = SM $ HM.mapMaybeWithKey f m


intersectionWith
  :: (Hashable k, Ord k)
  => (a1 -> b -> a) -> StrictMap k a1 -> StrictMap k b -> StrictMap k a
intersectionWith f (SM m) (SM m') = SM $ HM.intersectionWith f m m'

instance Functor (StrictMap k) where
  fmap f (SM m) = SM $ HM.map f m

instance Foldable (StrictMap k) where
  foldr f start = foldr f start . unSM

instance Traversable (StrictMap k) where
  traverse f (SM m) = SM <$> HM.traverseWithKey (const f) m

instance (Hashable k, Ord k) => Align (StrictMap k) where
  nil = mempty
  align (SM m) (SM m') = strictMap $ align m m'

instance (Hashable k, Ord k) => Trace (StrictMap k) where
  diagonal (SM m) = SM . force . diagonal $ unSM <$> m

instance (Hashable k, Ord k) => Semigroup (StrictMap k a) where
  SM m <> SM m' = SM $ HM.union m m'

instance (Hashable k, Ord k) => Monoid (StrictMap k a) where
  mempty = SM HM.empty
  mappend = (<>)

type instance Index (StrictMap k a) = k
type instance IxValue (StrictMap k a) = a


instance (Hashable k, Ord k) => FunctorWithIndex k (StrictMap k) where
  imap f = over _SM (HM.mapWithKey f)

instance (Hashable k, Ord k) => FoldableWithIndex k (StrictMap k) where
  ifoldMap f (SM m) = ifoldMap f m

instance (Hashable k, Ord k) => TraversableWithIndex k (StrictMap k) where
  itraverse f (SM m) = SM <$> itraverse f m

instance (Hashable k, Ord k) => At (StrictMap k a) where
  at k f (SM m) =
    f mv <&> \r -> case r of
      Nothing -> maybe (SM m) (const (SM $ HM.delete k m)) mv
      Just v' -> SM $ HM.insert k v' m
    where mv = HM.lookup k m


instance (Hashable k, Ord k) => Ixed (StrictMap k a) where
  ix k f (SM m) =
    case HM.lookup k m of
     Just v  -> f v <&> \v' -> SM (HM.insert k v' m)
     Nothing -> pure $ SM m


instance (Hashable k, Ord k) => IsList (StrictMap k a) where
  type Item (StrictMap k a) = (k, a)
  fromList = strictMap . fromList
  toList = toList . unSM


instance Show2 StrictMap where
  liftShowsPrec2 spk slk spv slv d (SM m) =
    showsUnaryWith (liftShowsPrec sp sl) "fromList" d (HM.toList m)
    where
      sp = liftShowsPrec2 spk slk spv slv
      sl = liftShowList2 spk slk spv slv

instance Show k => Show1 (StrictMap k) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
