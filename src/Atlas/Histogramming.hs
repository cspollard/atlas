{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Atlas.Histogramming
  ( Fills
  , Foldl, FoldM(..)
  , dsigdXpbY, dndx
  , mev, gev, rad, pt
  , channel, channelWithLabel, channelsWithLabels
  , hEmpty, hist1DDef, prof1DDef, hist2DDef
  , nH, ptH, etaH, lvHs, lvsHs
  , (=$<<), (<$=), prebind, liftAF
  , physObjH, foldedH
  -- , filterFolder, matchRegex
  ) where

import           Atlas.PhysObj
import           Atlas.Variation hiding (singleton)
import           Control.Foldl          (FoldM (..))
import qualified Control.Foldl          as F
import           Control.Lens
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.HEP.LorentzVector
import           Data.Hist
import qualified Data.Histogram.Generic as G
import           Data.Semigroup
import qualified Data.Text              as T
import           Data.Tuple             (swap)
import qualified Data.Vector            as V
import           Data.YODA.Obj


type Foldl = F.Fold
type Fills a = Foldl (PhysObj a) (Folder (Vars YodaObj))


prebind :: (Monad m, Profunctor p) => (a -> m b) -> p (m b) c -> p (m a) c
prebind g = lmap (>>= g)


infixl 2 =$<<
(=$<<) :: (Monad m, Profunctor p) => p (m b) c -> (a -> m b) -> p (m a) c
(=$<<) = flip prebind


infixl 2 <$=
(<$=) :: Profunctor p => p b c -> (a -> b) -> p a c
h <$= f = lmap f h


liftAF :: Applicative m => Foldl b c -> Foldl (m b) (m c)
liftAF (F.Fold comb start done) = F.Fold comb' start' done'
  where
    start' = pure start
    comb' xs x = comb <$> xs <*> x
    done' = fmap done



physObjH :: Foldl (a, Double) b -> Foldl (PhysObj a) (Vars b)
physObjH = lmap runPhysObj . liftAF . lmap go . F.handles _Just
  where
    go :: These Double a -> Maybe (a, Double)
    go = fmap swap . sequence . fromThese 1.0 Nothing . fmap Just


foldedH
  :: (Foldable f, Applicative f, Bitraversable t)
  => Foldl (t c d) c1 -> Foldl (t (f c) d) c1
foldedH f = F.handles folded f <$= bitraverse id pure


hEmpty :: (Bin bin, Monoid a) => bin -> Histogram V.Vector bin a
hEmpty b =
  let v = V.replicate (nBins b) mempty
      uo = Just (mempty, mempty)
  in G.histogramUO b uo v


hist1DDef
  :: (BinValue b ~ Double, IntervalBin b)
  => b -> T.Text -> T.Text -> Foldl (Double, Double) YodaObj
hist1DDef b xt yt =
  Annotated [("XLabel", xt), ("YLabel", yt)]
  . H1DD
  . over bins toArbBin
  <$> hist1DFill (hEmpty b)


hist2DDef
  :: (BinValue b ~ Double, IntervalBin b)
  => b -> b -> T.Text -> T.Text -> Foldl ((Double, Double), Double) YodaObj
hist2DDef bx by xt yt =
  Annotated [("XLabel", xt), ("YLabel", yt)]
  . H2DD
  . over bins (fmapBinX toArbBin)
  . over bins (fmapBinY toArbBin)
  <$> hist2DFill (hEmpty $ Bin2D bx by)


prof1DDef
  :: (BinValue b ~ Double, IntervalBin b)
  => b -> T.Text -> T.Text -> Foldl ((Double, Double), Double) YodaObj
prof1DDef b xt yt =
  Annotated [("XLabel", xt), ("YLabel", yt)]
  . P1DD
  . over bins toArbBin
  <$> prof1DFill (hEmpty b)


cut :: (Monoid c, MonadChronicle c m) => (a -> m Bool) -> a -> m a
cut c o = do
  p <- c o
  if p then return o else confess mempty


channel
  :: (Monoid c, MonadChronicle c m)
  => (a -> m Bool) -> Foldl (m a) r -> Foldl (m a) r
channel c = prebind (cut c)


channelWithLabel
  :: (Monoid c, MonadChronicle c m)
  => T.Text
  -> (a -> m Bool)
  -> Foldl (m a) (Folder b)
  -> Foldl (m a) (Folder b)
channelWithLabel n f = fmap (prefixF n) . channel f


channelsWithLabels
  :: (Monoid c, MonadChronicle c m, Semigroup b)
  => [(T.Text, a -> m Bool)]
  -> Foldl (m a) (Folder b)
  -> Foldl (m a) (Folder b)
channelsWithLabels fns fills =
  mconcat $ uncurry channelWithLabel <$> fns <*> pure fills


nH :: Foldable t => Int -> Foldl (t a, Double) (Folder YodaObj)
nH n =
  singleton "/n"
  <$> hist1DDef (binD 0 n (fromIntegral n)) "$n$" (dndx "n" "1")
  <$= first (fromIntegral . length)


ptH :: HasLorentzVector a => Foldl (a, Double) YodaObj
ptH =
  hist1DDef
    (logBinD 20 25 500) -- :: TransformedBin BinD (Log10BT Double))
    "$p_{\\mathrm T}$ [GeV]"
    (dndx pt gev)
    <$= first (view lvPt)


etaH :: HasLorentzVector a => Foldl (a, Double) YodaObj
etaH =
  hist1DDef (binD (-3) 39 3) "$\\eta$" (dndx "\\eta" "{\\mathrm rad}")
    <$= first (view lvEta)


lvHs :: HasLorentzVector a => Fills a
lvHs =
  mconcat
    [ singleton "/pt" <$> physObjH ptH
    , singleton "/eta" <$> physObjH etaH
    ]


lvsHs
  :: (Foldable f, Applicative f, HasLorentzVector a)
  => Fills (f a)
lvsHs =
  mconcat
  [ fmap (singleton "/pt") . physObjH
    $ F.handles folded ptH <$= bitraverse id pure
  , fmap (singleton "/eta") . physObjH
    $ F.handles folded etaH <$= bitraverse id pure
  ]


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


-- -- innerF :: Monad m => (a -> m b) -> Foldl (m b) c -> Foldl (m a) c
-- -- innerF g = F.premap (g =<<)
-- --
-- -- -- outerM :: Monad m => (a -> m b) -> Foldl (m b) (m c) -> Foldl (m a) (m c)
-- -- -- outerM g (Foldl comb start done) = Foldl comb' start' done'
-- -- --   where
-- -- --     start' = return start
-- -- --     done' x = done =<< x
-- -- --     comb' mx ma = flip comb (g =<< ma) <$> mx
-- -- --
-- -- -- outer :: Applicative m => (a -> m b) -> Foldl b c -> Foldl a (m c)
-- -- -- outer g = F.premap g . liftFA
