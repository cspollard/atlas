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
  ( Fills, AnaObjs, Folder
  , dsigdXpbY, dndx
  , mev, gev, rad, pt
  , layerVars, layerCut, physObjMoore
  , histo1DDef, histo2DDef, histo1DDef', histo2DDef'
  , nH, ptH, etaH, lvHs
  , (<$=), (=$>), (=$$>), (=$<<)
  , Histo1D, Histo2D
  , channel
  , module X
  ) where


import Data.Bifunctor
import Data.Tuple (swap)
import qualified Data.Map.Strict as M
import           Data.HEP.LorentzVector
import Control.Lens (view)
import Control.Category (Category)
import Data.Functor.Identity
import           Data.Binned as X
import           Data.Gauss as X
import Data.Profunctor
import Data.Annotated
import           Both
import           Moore as X
import qualified Data.Text              as T
import Atlas.PhysObj
import Atlas.Variation
import Atlas.ScaleFactor
import Data.StrictMap


type Folder = StrictMap T.Text


type Histo1D = Binned Double (Gauss Identity Double)
type Histo2D = Binned2D Double Double (Gauss TF Double)

type FAV a = Folder (Annotated (Vars a))
type AnaObjs = Both (FAV Histo1D) (FAV Histo2D)
type Fills a = Moore' (PhysObj a) AnaObjs


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



histo1DDef'
  :: [Double]
  -> T.Text
  -> T.Text
  -> Moore' (Double, Identity Double) (Annotated Histo1D)
histo1DDef' xs xt yt =
  annotated [("XLabel", xt), ("YLabel", yt)]
  <$> mooreHisto1D xs
  <$= swap


histo1DDef
  :: [Double]
  -> T.Text
  -> T.Text
  -> T.Text
  -> Fills Double
histo1DDef xs xt yt name =
  flip Both mempty . singleton name . sequenceA
  <$> physObjMoore h

  where
    h = lmap (second' Identity) $ histo1DDef' xs xt yt


histo2DDef'
  :: [Double]
  -> [Double]
  -> T.Text
  -> T.Text
  -> Moore' (Double, TF Double) (Annotated Histo2D)
histo2DDef' xs ys xt yt =
  annotated [("XLabel", xt), ("YLabel", yt)]
  <$> mooreHisto2D xs ys
  <$= swap


histo2DDef
  :: [Double]
  -> [Double]
  -> T.Text
  -> T.Text
  -> T.Text
  -> Fills (Double, Double)
histo2DDef xs ys xt yt name =
  Both mempty . singleton name . sequenceA
  <$> physObjMoore h

  where
    h = lmap (second' $ uncurry TF) $ histo2DDef' xs ys xt yt


-- needs work...
-- prof1DDef
--   :: [Double]
--   -> T.Text
--   -> T.Text
--   -> Moore' (Double, TF Double) (Annotated Prof1D)
-- prof1DDef xs xt yt =
--   Annotated [("XLabel", xt), ("YLabel", yt)]
--   <$> mooreProf1D xs
--   <$= swap


channel
  :: forall s f g a b. (Bifunctor f, Functor g, Semigroup s)
  => s -> g (f (StrictMap s a) (StrictMap s b)) -> g (f (StrictMap s a) (StrictMap s b))
channel s = fmap $ bimap go go
  where
    go :: forall x. StrictMap s x -> StrictMap s x
    go = liftSM $ M.mapKeysMonotonic (s <>)


nH :: Foldable t => Integer -> Fills (t x)
nH nmax =
  lmap (fmap $ fromIntegral . length)
  $ histo1DDef (evenBins' 0 nmax (fromIntegral nmax)) "$n$" (dndx "n" "1") "/n"


ptH :: HasLorentzVector a => Fills a
ptH =
  lmap (fmap $ view lvPt)
  $ histo1DDef (logBins' 20 25 500) "$p_{\\mathrm T}$ [GeV]" (dndx pt gev) "/pt"


etaH :: HasLorentzVector a => Fills a
etaH =
  lmap (fmap $ view lvEta)
  $ histo1DDef (evenBins' (-3) 39 3) "$\\eta$" (dndx "\\eta" "{\\mathrm rad}") "/eta"


lvHs
  :: HasLorentzVector a
  => Fills a
lvHs = ptH <> etaH



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
