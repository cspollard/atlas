{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Atlas.Histogramming
  ( module X
  , dsigdXpbY
  , mev, gev, rad, pt
  , Fill, channel, channels
  , hEmpty, hist1DDef, prof1DDef, hist2DDef
  , nH, ptH, etaH, lvHs
  , (<$=), (<$$=)
  ) where

import qualified Control.Foldl          as F
import           Control.Lens
import           Data.Atlas.Corrected
import           Data.HEP.LorentzVector as X
import           Data.Hist              as X
import qualified Data.Histogram.Generic as G
import qualified Data.Map.Strict        as M
import           Data.Semigroup
import           Data.Text              (Text)
import qualified Data.Vector            as V
import           Data.YODA.Obj          as X


dsigdXpbY :: Text -> Text -> Text
dsigdXpbY x y = "$\\frac{d\\sigma}{d" <> x <> "} \\frac{\\mathrm{pb}}{" <> y <> "}$"

mev, gev, rad, pt :: Text
mev = "\\mathrm{MeV}"
gev = "\\mathrm{GeV}"
rad = "\\mathrm{rad}"
pt = "p_{\\mathrm{T}}"


type Fill a = F.Fold (Corrected ScaleFactor a) YodaFolder


channel :: Text -> (a -> Bool) -> Fill a -> Fill a
channel n f fills = M.mapKeysMonotonic (n <>) <$> F.handles (selector (f.fst.runCorrected)) fills


channels :: [(Text, a -> Bool)] -> Fill a -> Fill a
channels fns fills = mconcat $ uncurry channel <$> fns <*> pure fills


hEmpty :: (Bin bin, Monoid a) => bin -> Histogram V.Vector bin a
hEmpty b =
  let v = V.replicate (nBins b) mempty
      uo = Just (mempty, mempty)
  in G.histogramUO b uo v

toCorrected :: F.Fold (a, b) r -> F.Fold (Corrected (Product b) a) r
toCorrected = F.premap (fmap getProduct . runCorrected)

hist1DDef
  :: (BinValue b ~ Double, IntervalBin b)
  => b -> Text -> Text -> Text -> Fill Double
hist1DDef b xt yt pa =
  M.singleton pa
    . Annotated [("XLabel", xt), ("YLabel", yt)]
    . H1DD
    . over bins toArbBin
    <$> toCorrected (hist1DFill (hEmpty b))

hist2DDef
  :: (BinValue b ~ Double, IntervalBin b)
  => b -> b -> Text -> Text -> Text -> Fill (Double, Double)
hist2DDef bx by xt yt pa =
  M.singleton pa
    . Annotated [("XLabel", xt), ("YLabel", yt)]
    . H2DD
    . over bins (fmapBinX toArbBin)
    . over bins (fmapBinY toArbBin)
    <$> toCorrected (hist2DFill (hEmpty (Bin2D bx by)))

prof1DDef
  :: (BinValue b ~ Double, IntervalBin b)
  => b -> Text -> Text -> Text -> Fill (Double, Double)
prof1DDef b xt yt pa =
  M.singleton pa
    . Annotated [("XLabel", xt), ("YLabel", yt)]
    . P1DD
    . over bins toArbBin
    <$> toCorrected (prof1DFill (hEmpty b))


nH :: Foldable f => Int -> Fill (f a)
nH n =
  F.premap (fmap $ fromIntegral . length)
    $ hist1DDef (binD 0 n (fromIntegral n)) "$n$" (dsigdXpbY "n" "1") "/n"

ptH :: HasLorentzVector a => Fill a
ptH =
  F.premap (fmap $ view lvPt)
    $ hist1DDef
      (binD 0 50 500)
      "$p_{\\mathrm T}$ [GeV]"
      (dsigdXpbY pt gev)
      "/pt"

etaH :: HasLorentzVector a => Fill a
etaH =
  F.premap (fmap $ view lvEta)
    $ hist1DDef
      (binD (-3) 39 3)
      "$\\eta$"
      (dsigdXpbY "\\eta" "{\\mathrm rad}")
      "/eta"


-- generic histograms for a lorentz vector
lvHs :: HasLorentzVector a => Fill a
lvHs = mappend <$> ptH <*> etaH

selector :: (a -> Bool) -> Prism' a a
selector f = prism' id $ \x -> if f x then Just x else Nothing

infixl 2 <$=
(<$=) :: Fill a -> (b -> a) -> Fill b
h <$= f = F.premap (fmap f) h

infixl 2 <$$=
(<$$=) :: Fill a -> (b -> Corrected ScaleFactor a) -> Fill b
h <$$= f = F.premap (f =<<) h
