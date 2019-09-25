{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Atlas.StrictMap
  ( Folder
  , StrictMap, strictMap, unSM, liftSM, liftSM2
  , singleton, inSM, smFromList
  , lookup, intersectionWith, mapMaybeWithKey
  ) where

import           Control.Lens
import           Data.Align
import           Data.Data
import           Data.Functor.Classes
import qualified Data.Map.Strict  as M
import           Data.Key
import           Data.Semigroup
import           Data.Serialize
import qualified Data.Text            as T
import           GHC.Exts             (IsList (..))
import           GHC.Generics
import           Linear.Matrix        (Trace (..))
import           Prelude              hiding (lookup)


type Folder = StrictMap T.Text


newtype StrictMap k a = SM { unSM :: M.Map k a }
  deriving (Show, Eq, Data, Typeable, Generic)

makePrisms ''StrictMap


strictMap :: M.Map k a -> StrictMap k a
strictMap = SM . force


force :: M.Map k a -> M.Map k a
force m = M.foldl' (flip seq) () m `seq` m


singleton :: k -> a -> StrictMap k a
singleton k = SM . M.singleton k


instance (Ord k, Serialize k, Serialize a)
  => Serialize (StrictMap k a) where
  put = put . M.toList . unSM
  get = strictMap . M.fromList <$> get


smFromList :: Ord k => [(k, a)] -> StrictMap k a
smFromList = fromList


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


type instance Key (StrictMap k) = k


instance Keyed (StrictMap k) where
  mapWithKey f (SM m) = SM $ M.mapWithKey f m


instance FoldableWithKey (StrictMap k) where
  foldMapWithKey f = foldMapWithKey f . unSM


mapMaybeWithKey :: (k -> a1 -> Maybe a) -> StrictMap k a1 -> StrictMap k a
mapMaybeWithKey f (SM m) = SM $ M.mapMaybeWithKey f m


intersectionWith
  :: Ord k
  => (a1 -> b -> a) -> StrictMap k a1 -> StrictMap k b -> StrictMap k a
intersectionWith f (SM m) (SM m') = SM $ M.intersectionWith f m m'

instance Functor (StrictMap k) where
  fmap f (SM m) = SM $ M.map f m

instance Foldable (StrictMap k) where
  foldr f start = foldr f start . unSM

instance Traversable (StrictMap k) where
  traverse f (SM m) = SM <$> M.traverseWithKey (const f) m

instance Ord k => Align (StrictMap k) where
  nil = mempty
  align (SM m) (SM m') = strictMap $ align m m'

instance Ord k => Trace (StrictMap k) where
  diagonal (SM m) = SM . force . diagonal $ unSM <$> m

instance Ord k => Semigroup (StrictMap k a) where
  SM m <> SM m' = SM $ M.union m m'

instance Ord k => Monoid (StrictMap k a) where
  mempty = SM M.empty
  mappend = (<>)

type instance Index (StrictMap k a) = k
type instance IxValue (StrictMap k a) = a


instance Ord k => FunctorWithIndex k (StrictMap k) where
  imap f = over _SM (M.mapWithKey f)

instance Ord k => FoldableWithIndex k (StrictMap k) where
  ifoldMap f (SM m) = ifoldMap f m

instance Ord k => TraversableWithIndex k (StrictMap k) where
  itraverse f (SM m) = SM <$> itraverse f m

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


instance Ord k => IsList (StrictMap k a) where
  type Item (StrictMap k a) = (k, a)
  fromList = strictMap . fromList
  toList = toList . unSM


instance Show2 StrictMap where
  liftShowsPrec2 spk slk spv slv d (SM m) =
    showsUnaryWith (liftShowsPrec sp sl) "fromList" d (M.toList m)
    where
      sp = liftShowsPrec2 spk slk spv slv
      sl = liftShowList2 spk slk spv slv

instance Show k => Show1 (StrictMap k) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
