{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Data.Atlas.Histogramming
  ( module X
  , dsigdXpbY
  , mev, gev, rad, pt
  , Fill, FillSimple, channel, channels
  , hEmpty, hist1DDef, prof1DDef, hist2DDef
  , nH, ptH, etaH, lvHs
  , (<$=), (<$$=), (<$$$=)
  , withVariations
  ) where

import qualified Control.Foldl          as F
import           Control.Lens
import           Control.Monad          (join)
import           Data.Atlas.Corrected
import           Data.Atlas.Variation
import           Data.HEP.LorentzVector as X
import           Data.Hist              as X
import qualified Data.Histogram.Generic as G
import qualified Data.Map.Strict        as M
import           Data.Semigroup
import qualified Data.Text              as T
import           Data.Tuple             (swap)
import qualified Data.Vector            as V
import           Data.YODA.Obj          as X


dsigdXpbY :: T.Text -> T.Text -> T.Text
dsigdXpbY x y =
  "$\\frac{d\\sigma}{d" <> x <> "} \\frac{\\mathrm{pb}}{" <> y <> "}$"

mev, gev, rad, pt :: T.Text
mev = "\\mathrm{MeV}"
gev = "\\mathrm{GeV}"
rad = "\\mathrm{rad}"
pt = "p_{\\mathrm{T}}"


type FillSimple a = F.Fold (Double, a) YodaFolder
type Fill a = F.Fold (Vars (Corrected ScaleFactor (Maybe a))) (Vars YodaFolder)


selector :: (a -> Bool) -> Prism' a a
selector f = prism' id $ \x -> if f x then Just x else Nothing

channel :: T.Text -> (a -> Bool) -> FillSimple a -> FillSimple a
channel n f fills =
  M.mapKeysMonotonic (n <>) <$> F.handles (selector (f.snd)) fills


channels :: [(T.Text, a -> Bool)] -> FillSimple a -> FillSimple a
channels fns fills = mconcat $ uncurry channel <$> fns <*> pure fills


hEmpty :: (Bin bin, Monoid a) => bin -> Histogram V.Vector bin a
hEmpty b =
  let v = V.replicate (nBins b) mempty
      uo = Just (mempty, mempty)
  in G.histogramUO b uo v


withVariations
  :: F.Fold (Double, a) b
  -> F.Fold (Vars (Corrected SF (Maybe a))) (Vars b)
withVariations (F.Fold comb start done) =
  F.premap (fmap $ sequence . swap . fmap runSF . runCorrected)
    $ F.Fold comb' (pure start) (fmap done)

  where
    -- TODO
    -- this pattern has come up more than once:
    -- we have a default value and something to zip,
    -- replacing any missing in the zip with the default...
    -- how to generalize?
    -- in the case of Variations we want the default to change along the way
    -- in this case we want the default to always be start'
    -- hmmmmmmmm

    mcomb h = maybe h (comb h)
    comb' (Variations n m) (Variations n' m') =
      Variations (mcomb n n') . variations
        $ mcomb <$> Variations start m <*> Variations Nothing m'


hist1DDef
  :: (BinValue b ~ Double, IntervalBin b)
  => b -> T.Text -> T.Text -> T.Text -> FillSimple Double
hist1DDef b xt yt pa =
  M.singleton pa
    . Annotated [("XLabel", xt), ("YLabel", yt)]
    . H1DD
    . over bins toArbBin
    <$> F.premap swap (hist1DFill (hEmpty b))

hist2DDef
  :: (BinValue b ~ Double, IntervalBin b)
  => b -> b -> T.Text -> T.Text -> T.Text -> FillSimple (Double, Double)
hist2DDef bx by xt yt pa =
  M.singleton pa
    . Annotated [("XLabel", xt), ("YLabel", yt)]
    . H2DD
    . over bins (fmapBinX toArbBin)
    . over bins (fmapBinY toArbBin)
    <$> F.premap swap (hist2DFill (hEmpty (Bin2D bx by)))

prof1DDef
  :: (BinValue b ~ Double, IntervalBin b)
  => b -> T.Text -> T.Text -> T.Text -> FillSimple (Double, Double)
prof1DDef b xt yt pa =
  M.singleton pa
    . Annotated [("XLabel", xt), ("YLabel", yt)]
    . P1DD
    . over bins toArbBin
    <$> F.premap swap (prof1DFill (hEmpty b))


nH :: Foldable f => Int -> FillSimple (f a)
nH n =
  hist1DDef (binD 0 n (fromIntegral n)) "$n$" (dsigdXpbY "n" "1") "/n"
    <$= fromIntegral . length

ptH :: HasLorentzVector a => FillSimple a
ptH =
  hist1DDef (binD 0 50 500) "$p_{\\mathrm T}$ [GeV]" (dsigdXpbY pt gev) "/pt"
    <$= view lvPt

etaH :: HasLorentzVector a => FillSimple a
etaH =
  hist1DDef
    (binD (-3) 39 3)
    "$\\eta$"
    (dsigdXpbY "\\eta" "{\\mathrm rad}")
    "/eta"
    <$= view lvEta


-- generic histograms for a lorentz vector
lvHs :: HasLorentzVector a => FillSimple a
lvHs = mappend <$> ptH <*> etaH


infixl 2 <$=
(<$=) :: FillSimple b -> (a -> b) -> FillSimple a
h <$= f = F.premap (fmap f) h

infixl 2 <$$=
(<$$=)
  :: (Monad m, Traversable m)
  => F.Fold (m c) b -> (a -> m c) -> F.Fold (m a) b
h <$$= f = h <$$$= (Identity . f)

infixl 2 <$$$=
(<$$$=)
  :: (Monad m, Traversable m, Applicative f, Foldable f)
  => F.Fold (m c) b -> (a -> f (m c)) -> F.Fold (m a) b
f <$$$= g = F.premap (reduce . fmap g) . F.handles folded $ f
  where
    reduce =  fmap join . sequenceA


-- we need the Maybe since the nominal may fail.
