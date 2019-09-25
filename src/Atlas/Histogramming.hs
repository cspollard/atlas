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
  ( Fills, FolderAV
  , dsigdXpbY, dndx
  , mev, gev, rad, pt
  , layerVars, layerCut, physObjMoore
  , hist1DDef, prof1DDef, hist2DDef
  , nH, ptH, etaH, lvHs
  , (<$=), (=$>), (=$$>), (=$<<)
  , Histo1D, Prof1D, Histo2D
  , module X
  ) where


import Data.Tuple (swap)
import           Data.HEP.LorentzVector
import Control.Lens (view)
import Control.Category (Category)
import Data.Functor.Identity
import           Data.Binned as X
import           Data.Gauss as X
import Data.Profunctor
import Data.Bitraversable (bisequenceA)
import Data.Annotated
import           Data.Both
import           Data.Moore as X
import qualified Data.Text              as T
import Atlas.PhysObj
import Atlas.Variation
import Atlas.ScaleFactor
import Atlas.StrictMap


type FolderAV a = Folder (Annotated (Vars a))

type Fills a b = Moore' (PhysObj a) (FolderAV b)


infixl 2 =$>
(=$>) :: Profunctor p => (a -> b) -> p b c -> p a c
(=$>) = lmap


infixl 2 <$=
(<$=) :: Profunctor p => p b c -> (a -> b) -> p a c
(<$=) = flip lmap


infixl 2 =$$>
(=$$>) :: (Profunctor p, Category p) => p i' i -> Moore p i o -> Moore p i' o
(=$$>) = premap


infixl 2 =$<<
(=$<<) :: (Profunctor p, Monad m) => p (m b) c -> (a -> m b) -> p (m a) c
h =$<< f = h <$= (>>= f)


layerVars :: [T.Text] -> Moore' a b -> Moore' (Vars a) (Vars b)
layerVars ts m = apply $ copyVars ts m


layerCut :: String -> Moore' a b -> Moore' c d -> Moore' (Either a c) (String, Both b d)
layerCut s mp mf = (s,) <$> layerEither (Both mp mf)


-- which variations are allowed?
physObjMoore :: Moore' (Double, a) b -> Moore' (PhysObj a) (Vars b)
physObjMoore m =
  premap (fmap swap . runPhysObj)
  . layerVars []
  . premap sequenceA
  . foldlMoore
  . premap (first' runSF)
  $ m


type Histo1D = Binned Double (Gauss Identity Double)
type Prof1D = Binned Double (Gauss TF Double)
type Histo2D = Binned Double (Binned Double (Gauss TF Double))


hist1DDef
  :: [Double]
  -> T.Text
  -> T.Text
  -> Moore' (Double, Identity Double) (Annotated Histo1D)
hist1DDef xs xt yt =
  Annotated [("XLabel", xt), ("YLabel", yt)]
  <$> mooreHisto1D xs
  <$= swap


hist2DDef
  :: [Double]
  -> [Double]
  -> T.Text
  -> T.Text
  -> Moore' (Double, TF Double) (Annotated Histo2D)
hist2DDef xs ys xt yt =
  Annotated [("XLabel", xt), ("YLabel", yt)]
  <$> mooreHisto2D xs ys
  <$= swap


prof1DDef
  :: [Double]
  -> T.Text
  -> T.Text
  -> Moore' (Double, TF Double) (Annotated Prof1D)
prof1DDef xs xt yt =
  Annotated [("XLabel", xt), ("YLabel", yt)]
  <$> mooreProf1D xs
  <$= swap


-- TODO
-- channels
-- should just be a layerF


nH :: Foldable t => Integer -> Moore' (Double, t x) (Annotated Histo1D)
nH nmax =
  hist1DDef (evenBins' 0 nmax (fromIntegral nmax)) "$n$" (dndx "n" "1")
  <$= fmap (fromIntegral . length)


ptH :: HasLorentzVector a => Moore' (Double, a) (Annotated Histo1D)
ptH =
  hist1DDef (logBins' 20 25 500) "$p_{\\mathrm T}$ [GeV]" (dndx pt gev)
  <$= fmap (Identity . view lvPt)


etaH :: HasLorentzVector a => Moore' (Double, a) (Annotated Histo1D)
etaH =
  hist1DDef (evenBins' (-3) 39 3) "$\\eta$" (dndx "\\eta" "{\\mathrm rad}")
  <$= fmap (Identity . view lvEta)


lvHs
  :: HasLorentzVector a
  => Moore' (Double, a) (Both (Annotated Histo1D) (Annotated Histo1D))
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
