{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Atlas.Histogramming
  ( dsigdXpbY, dndx
  , mev, gev, rad, pt
  , layerVars, layerCut
  , hist1DDef, prof1DDef, hist2DDef
  , nH, ptH, etaH, lvHs
  , (<$=), (=$>), (=$$>)
  ) where


import           Atlas.Variation        hiding (singleton)
import           Data.HEP.LorentzVector
import Control.Lens (view)
import Control.Category (Category)
import Data.Functor.Identity
import           Data.Binned
import           Data.Gauss
import Data.Profunctor
import Data.Bitraversable (bisequenceA)
import Data.Annotated
import           Data.Both
import           Data.Moore
import qualified Data.Text              as T


infixl 2 =$>
(=$>) :: Profunctor p => (a -> b) -> p b c -> p a c
(=$>) = lmap

infixl 2 <$=
(<$=) :: Profunctor p => p b c -> (a -> b) -> p a c
(<$=) = flip lmap


infixl 2 =$$>
(=$$>) :: (Profunctor p, Category p) => p i' i -> Moore p i o -> Moore p i' o
(=$$>) = premap


layerVars :: [T.Text] -> Moore' a b -> Moore' (Vars a) (Vars b)
layerVars ts m = apply $ copyVars ts m


layerCut :: String -> Moore' a b -> Moore' c d -> Moore' (Either a c) (String, Both b d)
layerCut s mp mf = (s,) <$> layerEither (Both mp mf)


type Histo1D = Binned Double (Gauss Identity Double)
type Prof1D = Binned Double (Gauss TF Double)
type Histo2D = Binned Double (Binned Double (Gauss TF Double))


hist1DDef
  :: [Double]
  -> T.Text
  -> T.Text
  -> Moore' (Identity Double, Double) (Annotated Histo1D)
hist1DDef xs xt yt =
  Annotated [("XLabel", xt), ("YLabel", yt)]
  <$> mooreHisto1D xs


hist2DDef
  :: [Double]
  -> [Double]
  -> T.Text
  -> T.Text
  -> Moore' (TF Double, Double) (Annotated Histo2D)
hist2DDef xs ys xt yt =
  Annotated [("XLabel", xt), ("YLabel", yt)]
  <$> mooreHisto2D xs ys


prof1DDef
  :: [Double]
  -> T.Text
  -> T.Text
  -> Moore' (TF Double, Double) (Annotated Prof1D)
prof1DDef xs xt yt =
  Annotated [("XLabel", xt), ("YLabel", yt)]
  <$> mooreProf1D xs


-- TODO
-- channels
-- should just be a layerF


nH :: Foldable t => Integer -> Moore' (t x, Double) (Annotated Histo1D)
nH nmax =
  hist1DDef (evenBins' 0 nmax (fromIntegral nmax)) "$n$" (dndx "n" "1")
  <$= first' (fromIntegral . length)


ptH :: HasLorentzVector a => Moore' (a, Double) (Annotated Histo1D)
ptH =
  hist1DDef (logBins' 20 25 500) "$p_{\\mathrm T}$ [GeV]" (dndx pt gev)
  <$= first' (Identity . view lvPt)


etaH :: HasLorentzVector a => Moore' (a, Double) (Annotated Histo1D)
etaH =
  hist1DDef (evenBins' (-3) 39 3) "$\\eta$" (dndx "\\eta" "{\\mathrm rad}")
  <$= first' (Identity . view lvEta)


lvHs
  :: HasLorentzVector a
  => Moore' (a, Double) (Both (Annotated Histo1D) (Annotated Histo1D))
lvHs = bisequenceA $ Both ptH etaH



dsigdXpbY :: T.Text -> T.Text -> T.Text
dsigdXpbY x y =
  "$\\frac{d\\sigma}{d" <> x <> "} \\frac{\\mathrm{pb}}{" <> y <> "}$"

dndx :: T.Text -> T.Text -> T.Text
dndx x y =
  "$\\frac{dN}{d" <> x <> "} \\frac{1}{" <> y <> "}$"

mev, gev, rad, pt :: T.Text
mev = "\\mathrm{MeV}"
gev = "\\mathrm{GeV}"
rad = "\\mathrm{rad}"
pt = "p_{\\mathrm{T}}"
