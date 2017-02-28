{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Atlas.Histogramming
  ( module X
  , dsigdXpbY
  , mev, gev, rad, pt
  , Fill, channel, channels
  , hEmpty, hist1DDef, prof1DDef
  , nH, ptH, etaH, lvHs
  , (<$$=)
  ) where

import qualified Control.Foldl          as F
import           Control.Lens
import           Data.Bifunctor
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


type Fill a = F.Fold (a, Double) YodaFolder

channel :: Text -> (a -> Bool) -> Fill a -> Fill a
channel n f fills = M.mapKeysMonotonic (n <>) <$> F.handles (selector (f.fst)) fills


channels :: [(Text, a -> Bool)] -> Fill a -> Fill a
channels fns fills = mconcat $ uncurry channel <$> fns <*> pure fills


hEmpty :: (Bin bin, Monoid a) => bin -> Histogram V.Vector bin a
hEmpty b =
  let v = V.replicate (nBins b) mempty
      uo = Just (mempty, mempty)
  in G.histogramUO b uo v

hist1DDef
  :: (BinValue b ~ Double, IntervalBin b)
  => b -> Text -> Text -> Text -> F.Fold (Double, Double) YodaFolder
hist1DDef b xt yt pa =
    M.singleton pa
      . Annotated [("XLabel", xt), ("YLabel", yt)]
      . H1DD
      . over bins toArbBin
      <$> hist1DFill (hEmpty b)

prof1DDef
  :: (BinValue b ~ Double, IntervalBin b)
  => b -> Text -> Text -> Text -> F.Fold ((Double, Double), Double) YodaFolder
prof1DDef b xt yt pa =
    M.singleton pa
      . Annotated [("XLabel", xt), ("YLabel", yt)]
      . P1DD
      . over bins toArbBin
      <$> prof1DFill (hEmpty b)


nH :: Foldable f => Int -> Fill (f a)
nH n =
  F.premap (first $ fromIntegral . length)
    $ hist1DDef (binD 0 n (fromIntegral n)) "$n$" (dsigdXpbY "n" "1") "/n"

ptH :: HasLorentzVector a => Fill a
ptH =
  F.premap (first $ view lvPt)
    $ hist1DDef
      (binD 0 50 500)
      "$p_{\\mathrm T}$ [GeV]"
      (dsigdXpbY pt gev)
      "/pt"

etaH :: HasLorentzVector a => Fill a
etaH =
  F.premap (first $ view lvEta)
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

infixl 2 <$$=
(<$$=) :: Field1 s b s1 b1 => F.Fold b r -> Getting b1 s1 b1 -> F.Fold s r
f <$$= h = F.premap (over _1 (view h)) f
