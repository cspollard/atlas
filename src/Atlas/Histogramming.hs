{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Atlas.Histogramming
  ( Foldl, FoldM(..), Fill, VarHist, VarHists, VarFill, VarFills
  , dsigdXpbY, dndx
  , mev, gev, rad, pt
  , channel, channelWithLabel, channelsWithLabels
  , hEmpty, hist1DDef, prof1DDef, hist2DDef
  , nH, ptH, etaH, lvHs
  , (=$<<), (<$=), prebind, liftAF
  , physObjH, foldedH
  -- , filterFolder, matchRegex
  ) where


import           Atlas.PhysObj
import           Atlas.ScaleFactor
import           Atlas.Variation        hiding (singleton)
import           Control.Applicative
import           Control.Foldl          (FoldM (..))
import qualified Control.Foldl          as F
import           Control.Lens
import           Control.Monad          (guard)
import           Data.Bitraversable
import           Data.HEP.LorentzVector
import           Data.Hist
import qualified Data.Histogram.Generic as G
import           Data.Semigroup
import qualified Data.Text              as T
import qualified Data.Vector            as V
import           Data.YODA.Obj


type Foldl = F.Fold
type Fill a = Foldl (a, Double) YodaObj
type VarHist = Annotated (Vars Obj)
type VarHists = Folder VarHist
type VarFill a = Foldl (PhysObj a) VarHist
type VarFills a = Foldl (PhysObj a) VarHists


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
physObjH = lmap ((fmap.fmap) runSF . runPhysObj) . liftAF . lmap go . F.handles _Just
  where
    -- TODO
    -- inefficiencies really should play a role somehow...
    go (Nothing, _) = Nothing
    go (Just x, y)  = Just (x, y)


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
  => b -> T.Text -> T.Text -> VarFill Double
hist1DDef b xt yt =
  Annotated [("XLabel", xt), ("YLabel", yt)]
  . fmap (H1DD . over bins toArbBin)
  <$> physObjH (hist1DFill (hEmpty b))


hist2DDef
  :: (BinValue b ~ Double, IntervalBin b)
  => b -> b -> T.Text -> T.Text -> VarFill (Double, Double)
hist2DDef bx by xt yt =
  Annotated [("XLabel", xt), ("YLabel", yt)]
  . fmap (H2DD . over bins (fmapBinX toArbBin . fmapBinY toArbBin))
  <$> physObjH (hist2DFill (hEmpty $ Bin2D bx by))


prof1DDef
  :: (BinValue b ~ Double, IntervalBin b)
  => b -> T.Text -> T.Text -> VarFill (Double, Double)
prof1DDef b xt yt =
  Annotated [("XLabel", xt), ("YLabel", yt)]
  . fmap (P1DD . over bins toArbBin)
  <$> physObjH (prof1DFill (hEmpty b))


cut :: (Alternative m, Monad m) => (a -> m Bool) -> a -> m ()
cut c o = do
  p <- c o
  guard p


channel
  :: (Alternative m, Monad m)
  => (a -> m Bool) -> Foldl (m a) r -> Foldl (m a) r
channel c = prebind (\x -> cut c x >> return x)


channelWithLabel
  :: (Alternative m, Monad m)
  => T.Text
  -> (a -> m Bool)
  -> Foldl (m a) (Folder b)
  -> Foldl (m a) (Folder b)
channelWithLabel n f = fmap (prefixF n) . channel f


channelsWithLabels
  :: (Alternative m, Monad m, Semigroup b)
  => [(T.Text, a -> m Bool)]
  -> Foldl (m a) (Folder b)
  -> Foldl (m a) (Folder b)
channelsWithLabels fns fills =
  mconcat $ uncurry channelWithLabel <$> fns <*> pure fills


nH :: Foldable t => Int -> VarFill (t a)
nH n =
  hist1DDef (binD 0 n (fromIntegral n)) "$n$" (dsigdXpbY "n" "1")
  <$= fmap (fromIntegral . length)


ptH :: HasLorentzVector a => VarFill a
ptH =
  hist1DDef
    (logBinD 30 20 250) -- :: TransformedBin BinD (Log10BT Double))
    "$p_{\\mathrm T}$ [GeV]"
    (dsigdXpbY pt gev)
    <$= fmap (view lvPt)


etaH :: HasLorentzVector a => VarFill a
etaH =
  hist1DDef (binD (-3) 39 3) "$\\eta$" (dsigdXpbY "\\eta" "{\\mathrm rad}")
    <$= fmap (view lvEta)


lvHs :: HasLorentzVector a => VarFills a
lvHs =
  mconcat
  [ singleton "/pt" <$> ptH
  , singleton "/eta" <$> etaH
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


-- innerF :: Monad m => (a -> m b) -> Foldl (m b) c -> Foldl (m a) c
-- innerF g = F.premap (g =<<)
--
-- -- outerM :: Monad m => (a -> m b) -> Foldl (m b) (m c) -> Foldl (m a) (m c)
-- -- outerM g (Foldl comb start done) = Foldl comb' start' done'
-- --   where
-- --     start' = return start
-- --     done' x = done =<< x
-- --     comb' mx ma = flip comb (g =<< ma) <$> mx
-- --
-- -- outer :: Applicative m => (a -> m b) -> Foldl b c -> Foldl a (m c)
-- -- outer g = F.premap g . liftFA
