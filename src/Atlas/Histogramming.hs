{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Atlas.Histogramming
  ( Fill, Fills
  , dsigdXpbY, dndx
  , mev, gev, rad, pt
  , channel, channelWithLabel, channelsWithLabels
  , hEmpty, hist1DDef, prof1DDef, hist2DDef
  , nH, ptH, etaH, lvHs
  , bindF, apF
  , (<$=)
  , filterFolder, matchRegex
  ) where

import           Atlas.PhysObj
import           Atlas.Variation
import           Control.Foldl             (FoldM (..))
import qualified Control.Foldl             as F
import           Control.Lens
import           Control.Monad.Fail        as MF
import           Data.HEP.LorentzVector
import           Data.Hist
import qualified Data.Histogram.Generic    as G
import qualified Data.Map.Strict           as M
import           Data.Semigroup
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import           Data.YODA.Obj
import           Text.Regex.Base.RegexLike
import           Text.Regex.Posix.String


type Foldl = F.Fold
type Fill a = FoldM Vars a YodaObj
type Fills a = FoldM Vars a (Folder YodaObj)


bindF :: Monad m => FoldM m a b -> (c -> m a) -> FoldM m c b
bindF (FoldM comb start done) f = FoldM comb' start done
  where
    comb' x c = comb x =<< f c


apF :: Applicative m => Foldl b c -> Foldl (m b) (m c)
apF (F.Fold comb start done) = F.Fold comb' start' done'
  where
    start' = pure start
    comb' xs x = comb <$> xs <*> x
    done' = fmap done


cut :: MonadFail m => (a -> m Bool) -> m a -> m a
cut c o = do
  p <- c =<< o
  if p then o else MF.fail "fail cut"


channel :: MonadFail m => (a -> m Bool) -> FoldM m a r -> FoldM m a r
channel c f = bindF f (cut c)


channelWithLabel
  :: MonadFail m
  => T.Text
  -> (a -> m Bool)
  -> FoldM m a (Folder b)
  -> FoldM m a (Folder b)
channelWithLabel n f = fmap (prefixF n) . channel f


channelsWithLabels
  :: (MonadFail m, Semigroup b)
  => [(T.Text, a -> m Bool)]
  -> Foldl (m a) (Folder b)
  -> Foldl (m a) (Folder b)
channelsWithLabels fns fills =
  mconcat $ uncurry channelWithLabel <$> fns <*> pure fills


innerF :: Monad m => (a -> m b) -> Foldl (m b) c -> Foldl (m a) c
innerF g = F.premap (g =<<)

-- outerM :: Monad m => (a -> m b) -> Foldl (m b) (m c) -> Foldl (m a) (m c)
-- outerM g (Foldl comb start done) = Foldl comb' start' done'
--   where
--     start' = return start
--     done' x = done =<< x
--     comb' mx ma = flip comb (g =<< ma) <$> mx
--
-- outer :: Applicative m => (a -> m b) -> Foldl b c -> Foldl a (m c)
-- outer g = F.premap g . liftFA

hEmpty :: (Bin bin, Monoid a) => bin -> Histogram V.Vector bin a
hEmpty b =
  let v = V.replicate (nBins b) mempty
      uo = Just (mempty, mempty)
  in G.histogramUO b uo v

pohelper :: Foldl (a, Double) c -> Foldl (PhysObj a) (Vars c)
pohelper =
  F.premap runPhysObj
  -- Foldl (Vars (Maybe (a, Double))) (Vars c)
  . apF
  -- Foldl (Maybe (a, Double)) c
  . F.handles folded
  -- Foldl (a, Double) c

hist1DDef
  :: (BinValue b ~ Double, IntervalBin b)
  => b -> T.Text -> T.Text -> Fill Double
hist1DDef b xt yt =
  fmap
    ( Annotated [("XLabel", xt), ("YLabel", yt)]
      . H1DD
      . over bins toArbBin
    )
  <$> (pohelper . hist1DFill $ hEmpty b)

hist2DDef
  :: (BinValue b ~ Double, IntervalBin b)
  => b -> b -> T.Text -> T.Text -> Fill (Double, Double)
hist2DDef bx by xt yt =
  fmap
    ( Annotated [("XLabel", xt), ("YLabel", yt)]
      . H2DD
      . over bins (fmapBinX toArbBin)
      . over bins (fmapBinY toArbBin)
    )
  <$> (pohelper .  hist2DFill . hEmpty $ Bin2D bx by)

prof1DDef
  :: (BinValue b ~ Double, IntervalBin b)
  => b -> T.Text -> T.Text -> Fill (Double, Double)
prof1DDef b xt yt =
  fmap
    ( Annotated [("XLabel", xt), ("YLabel", yt)]
      . P1DD
      . over bins toArbBin
    )
  <$> (pohelper . prof1DFill $ hEmpty b)

nH
  :: Foldable f
  => Int -> Fills (f a)
nH n =
  singleton "/n"
  <$> hist1DDef (binD 0 n (fromIntegral n)) "$n$" (dndx "n" "1")
  <$= fromIntegral . length

ptH :: HasLorentzVector a => Fills a
ptH =
  singleton "/pt"
  <$> hist1DDef
      (logBinD 20 25 500) -- :: TransformedBin BinD (Log10BT Double))
      "$p_{\\mathrm T}$ [GeV]"
      (dndx pt gev)
  <$= view lvPt

etaH :: HasLorentzVector a => Fills a
etaH =
  singleton "/eta"
  <$> hist1DDef (binD (-3) 39 3) "$\\eta$" (dndx "\\eta" "{\\mathrm rad}")
  <$= view lvEta

lvHs
  :: HasLorentzVector a => Fills a
lvHs = ptH `mappend` etaH


infixl 2 <$=
(<$=) :: Functor f => Foldl (f c) b -> (a -> c) -> Foldl (f a) b
h <$= f = F.premap (fmap f) h

matchRegex :: String -> String -> Bool
matchRegex rxp = matchTest (makeRegex rxp :: Regex)

filterFolder :: Maybe String -> Folder a -> Folder a
filterFolder s f = maybe f (`g` f) s
  where
    g s' =
      let rxp = makeRegex s' :: Regex
          h k _ = matchTest rxp $ T.unpack k
      in inF (M.filterWithKey h)


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
