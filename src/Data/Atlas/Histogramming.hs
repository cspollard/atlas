{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Atlas.Histogramming where

import Control.Lens
import qualified Control.Foldl as F

import Data.Semigroup

import Data.Maybe (listToMaybe)
import Data.Foldable (toList)

import Data.Text (Text)

import Data.YODA.Obj

toFold :: (b -> a -> b) -> b -> F.Fold a b
toFold f o = F.Fold f o id

feed :: F.Fold a b -> a -> F.Fold a b
feed (F.Fold f o g) x = let o' = f o x in o' `seq` F.Fold f o' g

lseq :: [a] -> [a]
lseq [] = []
lseq (x:xs) = seq x $ x : lseq xs


foldAll :: F.Foldable f => F.Fold a b -> F.Fold (f a) b
foldAll = F.handles folded

foldFirst :: F.Foldable f => F.Fold a b -> F.Fold (f a) b
foldFirst f = F.premap (listToMaybe . toList) (foldAll f)

foldIf :: (a -> Bool) -> F.Fold a b -> F.Fold a b
foldIf g f = F.premap g' $ foldAll f
    where g' x = if g x then Just x else Nothing

fillOver :: Fillable a => Traversal' b a -> b -> F.Fold (Weight a, FillVec a) b
fillOver l = toFold (\y (w, x) -> over l (filling w x) y)

-- not sure about these fixities.
infixl 2 <$=
(<$=) :: F.Fold c b -> (a -> c) -> F.Fold a b
(<$=) = flip F.premap

infixr 3 =:=
(=:=) :: Applicative f => f a -> f [a] -> f [a]
fx =:= fxs = (:) <$> fx <*> fxs


infixr 3 =++=
(=++=) :: Applicative f => f [a] -> f [a] -> f [a]
fxs =++= fys = (++) <$> fxs <*> fys


dsigdXpbY :: Text -> Text -> Text
dsigdXpbY x y = "$\\frac{d\\sigma}{d" <> x <> "} \\frac{\\mathrm{pb}}{" <> y <> "}$"

mev, gev, rad, pt :: Text
mev = "\\mathrm{MeV}"
gev = "\\mathrm{GeV}"
rad = "\\mathrm{rad}"
pt = "p_{\\mathrm{T}}"


yodaHist :: Int -> Double -> Double -> Text -> Text -> YodaObj
yodaHist nb xmin xmax xl yl =
    yodaHist1D nb xmin xmax
        & annots . at "XLabel" ?~ xl
        & annots . at "YLabel" ?~ yl

yodaProf :: Int -> Double -> Double -> Text -> Text -> YodaObj
yodaProf nb xmin xmax xl yl =
    yodaProf1D nb xmin xmax
        & annots . at "XLabel" ?~ xl
        & annots . at "YLabel" ?~ yl
