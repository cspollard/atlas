{-# OPTIONS_GHC -fno-warn-orphans #-}


-- A lot of this was inspired by
-- https://hackage.haskell.org/package/total-map-0.0.6/

module Data.Atlas.Variation
  -- ( VariationT, Variation, variationT, variation
  -- , mapVariationT, mapVariation
  -- , runVariationT, runVariation
  -- , NominalT, Nominal
  -- ) where
  where

import           Control.Applicative
import           Data.Functor.Classes
import qualified Data.Map.Strict      as M
import           Data.Maybe           (fromMaybe)
import qualified Data.Text            as T

data Variations k a =
  Variations
    { nominal    :: a
    , variations :: M.Map k a
    } deriving Show


instance Ord k => Functor (Variations k) where
  fmap f (Variations n m) = Variations (f n) (fmap f m)


instance Show k => Show1 (Variations k) where
  liftShowsPrec sp sl d (Variations n m) =
    showsBinaryWith sp (liftShowsPrec sp sl) "Variations" d n m

instance (Ord k, Monoid a) => Monoid (Variations k a) where
  mempty = pure mempty
  mappend = liftA2 mappend



instance Ord k => Applicative (Variations k) where
  pure x = Variations x M.empty
  -- if the same variation appears in both maps
  -- then we apply the function to the corresponding value
  -- otherwise "fill in" with the nominal value
  Variations f fs <*> Variations x xs =
    Variations
      (f x)
      ( M.mergeWithKey
        (\_ g -> Just . g)
        (fmap ($ x))
        (fmap f)
        fs
        xs
      )

instance Ord k => Monad (Variations k) where
  return = pure
  m >>= f = joinV (f <$> m)

(!) :: Ord k => Variations k a -> k -> a
Variations n m ! k = fromMaybe n (M.lookup k m)

-- how we join variations:
-- nominal -> nominal
-- if we have "on-diagonal" elements of mm, use them
-- else use nominal <*> varied from mm
-- else use varied <*> nominal
joinV :: Ord k => Variations k (Variations k v) -> Variations k v
joinV (Variations (Variations n m) mm) =
  Variations n (M.mapWithKey (flip (!)) mm `M.union` m)

type Vars = Variations T.Text

instance Show2 M.Map where
  liftShowsPrec2 spk slk spv slv d m =
    showsUnaryWith (liftShowsPrec sp sl) "fromList" d (M.toList m)
    where
      sp = liftShowsPrec2 spk slk spv slv
      sl = liftShowList2 spk slk spv slv

instance Show k => Show1 (M.Map k) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
