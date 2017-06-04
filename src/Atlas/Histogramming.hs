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
  , nH, ptH, etaH, lvHs
  , (=$<<), (<$=), prebind
  , physObjH
  -- , filterFolder, matchRegex
  ) where

import           Atlas.PhysObj
import           Atlas.Variation
import           Control.Foldl          (FoldM (..))
import qualified Control.Foldl          as F
import           Control.Lens
import           Control.Monad.Fail     as MF
import           Data.Bifunctor
import           Data.HEP.LorentzVector
import           Data.Hist
import qualified Data.Histogram.Generic as G
import           Data.Semigroup
import qualified Data.Text              as T
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


apF :: Applicative m => Foldl b c -> Foldl (m b) (m c)
apF (F.Fold comb start done) = F.Fold comb' start' done'
  where
    start' = pure start
    comb' xs x = comb <$> xs <*> x
    done' = fmap done


physObjH :: Foldl (a, Double) b -> Foldl (PhysObj a) (Vars b)
physObjH h = lmap runPhysObj . apF $ F.handles _Just h


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


cut :: MonadFail m => (a -> m Bool) -> a -> m a
cut c o = do
  p <- c o
  if p then return o else MF.fail "fail cut"


channel :: MonadFail m => (a -> m Bool) -> Foldl (m a) r -> Foldl (m a) r
channel c = prebind (cut c)


channelWithLabel
  :: MonadFail m
  => T.Text
  -> (a -> m Bool)
  -> Foldl (m a) (Folder b)
  -> Foldl (m a) (Folder b)
channelWithLabel n f = fmap (prefixF n) . channel f


channelsWithLabels
  :: (MonadFail m, Semigroup b)
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


lvHs :: HasLorentzVector a => Foldl (a, Double) (Folder YodaObj)
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


--
--
-- --
-- --
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
-- --
-- -- pohelper :: Foldl (a, Double) c -> Foldl (PhysObj a) (Vars c)
-- -- pohelper =
-- --   F.premap runPhysObj
-- --   -- Foldl (Vars (Maybe (a, Double))) (Vars c)
-- --   . apF
-- --   -- Foldl (Maybe (a, Double)) c
-- --   . F.handles folded
-- --   -- Foldl (a, Double) c
--
--
--
-- --
-- --
