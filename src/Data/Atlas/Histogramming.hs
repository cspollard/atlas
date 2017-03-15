{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Data.Atlas.Histogramming
  ( module X
  , Foldl, FoldlA
  , dsigdXpbY
  , mev, gev, rad, pt
  , channel, channelWithLabel, channelsWithLabels
  , hEmpty, hist1DDef, prof1DDef, hist2DDef
  , nH, ptH, etaH
  , (<$=), (<$$=)
  , inner, outerM
  ) where

import qualified Control.Foldl          as F
import           Control.Lens
import           Data.HEP.LorentzVector as X
import           Data.Hist              as X
import qualified Data.Histogram.Generic as G
import           Data.Semigroup
import qualified Data.Text              as T
import           Data.Tuple             (swap)
import qualified Data.Vector            as V
import           Data.YODA.Obj          as X


type Foldl = F.Fold
type FoldlA a b = forall f. Applicative f => Foldl a (f b)

dsigdXpbY :: T.Text -> T.Text -> T.Text
dsigdXpbY x y =
  "$\\frac{d\\sigma}{d" <> x <> "} \\frac{\\mathrm{pb}}{" <> y <> "}$"

mev, gev, rad, pt :: T.Text
mev = "\\mathrm{MeV}"
gev = "\\mathrm{GeV}"
rad = "\\mathrm{rad}"
pt = "p_{\\mathrm{T}}"


selector :: (a -> Bool) -> Prism' a a
selector f = prism' id $ \x -> if f x then Just x else Nothing

channel :: (a -> Bool) -> Foldl a b -> Foldl a b
channel f = F.handles $ selector f

channelWithLabel
  :: T.Text -> (a -> Bool) -> Foldl a (Folder b) -> Foldl a (Folder b)
channelWithLabel n g = fmap (prefixF n) . channel g

channelsWithLabels
  :: Semigroup b
  => [(T.Text, a -> Bool)] -> Foldl a (Folder b) -> Foldl a (Folder b)
channelsWithLabels fns fills =
  mconcat $ uncurry channelWithLabel <$> fns <*> pure fills


inner :: Monad m => (a -> m b) -> Foldl (m b) c -> Foldl (m a) c
inner g = F.premap (g =<<)

outerM :: Monad m => (a -> m b) -> Foldl (m b) (m c) -> Foldl (m a) (m c)
outerM g (F.Fold comb start done) = F.Fold comb' start' done'
  where
    start' = return start
    done' x = done =<< x
    comb' mx ma = flip comb (g =<< ma) <$> mx

-- outer :: Applicative m => (a -> m b) -> Foldl b c -> Foldl a (m c)
-- outer g = F.premap g . liftFA


pureFA :: Applicative m => Foldl b c -> Foldl b (m c)
pureFA (F.Fold comb start done) = F.Fold comb' start' done'
  where
    start' = pure start
    comb' xs x = comb <$> xs <*> pure x
    done' = fmap done


hEmpty :: (Bin bin, Monoid a) => bin -> Histogram V.Vector bin a
hEmpty b =
  let v = V.replicate (nBins b) mempty
      uo = Just (mempty, mempty)
  in G.histogramUO b uo v


-- TODO
-- I think it's time to upgrade to FoldMs, but...
hist1DDef
  :: (BinValue b ~ Double, IntervalBin b)
  => b -> T.Text -> T.Text -> FoldlA (Double, Double) YodaObj
hist1DDef b xt yt =
  fmap
    ( Annotated [("XLabel", xt), ("YLabel", yt)]
      . H1DD
      . over bins toArbBin
    )
  <$> F.premap swap (pureFA $ hist1DFill (hEmpty b))

hist2DDef
  :: (BinValue b ~ Double, IntervalBin b)
  => b -> b -> T.Text -> T.Text -> FoldlA (Double, (Double, Double)) YodaObj
hist2DDef bx by xt yt =
  fmap
    ( Annotated [("XLabel", xt), ("YLabel", yt)]
      . H2DD
      . over bins (fmapBinX toArbBin)
      . over bins (fmapBinY toArbBin)
    )
  <$> F.premap swap (pureFA $ hist2DFill (hEmpty (Bin2D bx by)))

prof1DDef
  :: (BinValue b ~ Double, IntervalBin b)
  => b -> T.Text -> T.Text -> FoldlA (Double, (Double, Double)) YodaObj
prof1DDef b xt yt =
  fmap
    ( Annotated [("XLabel", xt), ("YLabel", yt)]
      . P1DD
      . over bins toArbBin
    )
  <$> F.premap swap (pureFA $ prof1DFill (hEmpty b))

nH :: Applicative m => Int -> Foldl (Double, Int) (Folder (m YodaObj))
nH n =
  singleton "/n"
  <$> hist1DDef (binD 0 n (fromIntegral n)) "$n$" (dsigdXpbY "n" "1")
  <$= fromIntegral

ptH :: Applicative m => Foldl (Double, Double) (Folder (m YodaObj))
ptH =
  singleton "/pt"
  <$> hist1DDef (binD 0 50 500) "$p_{\\mathrm T}$ [GeV]" (dsigdXpbY pt gev)

etaH :: Applicative m => Foldl (Double, Double) (Folder (m YodaObj))
etaH =
  singleton "/eta"
  <$> hist1DDef (binD (-3) 39 3) "$\\eta$" (dsigdXpbY "\\eta" "{\\mathrm rad}")


infixl 2 <$=
(<$=) :: Functor f => Foldl (f c) b -> (a -> c) -> Foldl (f a) b
h <$= f = F.premap (fmap f) h

infixl 2 <$$=
(<$$=) :: (Functor f, Functor g) => Foldl (f (g c)) b -> (a -> c) -> Foldl (f (g a)) b
h <$$= f = F.premap ((fmap.fmap) f) h


-- withVariations
--   :: Foldl a b
--   -> Foldl (Vars a) (Vars b)
-- withVariations (Foldl comb start done) =
--   Foldl comb' (pure start) (fmap done)
--
--   where
--     -- TODO
--     -- this pattern has come up more than once:
--     -- we have a default value and something to zip,
--     -- replacing any missing in the zip with the default...
--     -- how to generalize?
--     -- in the case of Variations we want the default to change along the way
--     -- in this case we want the default to always be start
--     -- hmmmmmmmm
--
--     comb' (Variations n m) (Variations n' m') =
--       Variations (comb n n') . variations
--         $ comb <$> Variations start m <*> Variations n' m'
--
-- withSFs :: Foldl (Double, a) b -> Foldl (Corrected SF a) b
-- withSFs = F.premap $ swap . fmap runSF . runCorrected
--
-- -- any cuts/changes/etc coming before this point will be imposed
-- -- *directly* on the object of type a.
-- -- anything after this point will have to treat the as separately
-- -- and probably take a performance hit.
-- withVariedSFs
--   :: Foldl (Double, a) b -> Foldl (Corrected (Vars SF) a) (Vars b)
-- withVariedSFs =
--   F.premap (fmap swap . sequence . (fmap.fmap) runSF . runCorrected)
--     . withVariations
--
-- withVarsAndSFs
--   :: Foldl (Double, a) b
--   -> Foldl (Vars (Corrected SF a)) (Vars b)
-- withVarsAndSFs = withVariations . withSFs
--
--
--
-- -- TODO
-- -- if we could have a Folder of Vars of YodaObjs
-- -- then I don't think we would have duplicates of histograms that
-- -- aren't sensitive to variations!
-- -- if I understand correctly, these duplicates are created
-- -- *at the end of the folds*, i.e. when the monoid instance
-- -- of Fold kicks in.
--
--
--
--
-- infixl 2 <$$=
-- (<$$=)
--   :: (Monad m, Traversable m)
--   => Foldl (m c) b -> (a -> m c) -> Foldl (m a) b
-- h <$$= f = h <$$$= (Identity . f)
--
-- infixl 2 <$$$=
-- (<$$$=)
--   :: (Monad m, Traversable m, Applicative f, Foldable f)
--   => Foldl (m c) b -> (a -> f (m c)) -> Foldl (m a) b
-- f <$$$= g = F.premap (reduce . fmap g) . F.handles folded $ f
--   where
--     reduce =  fmap join . sequenceA
--
--
-- -- -- TODO
-- -- -- just some testing
-- -- data Jet =
-- --   Jet
-- --     { isBTagged :: Corrected (Vars SF) Bool
-- --     , jpt       :: Vars Double
-- --     , jeta      :: Double
-- --     } deriving Show
-- --
-- -- js :: [Jet]
-- -- js =
-- --   [ Jet
-- --     (withCorrection (True, Variations (sf "hey" 1.2) M.empty))
-- --     (Variations 27 [("jes", 29)])
-- --     1.5
-- --   , Jet
-- --     (withCorrection (True, Variations (sf "hey2" 1.3) M.empty))
-- --     (Variations 40 [("jes", 45)])
-- --     (-0.75)
-- --   ]
-- --
-- -- jetaH :: Foldl (Double, Jet) YodaFolder
-- -- jetaH =
-- --   hist1DDef
-- --     (binD (-3) 39 3)
-- --     "$\\eta$"
-- --     (dsigdXpbY "\\eta" "{\\mathrm rad}")
-- --     "/eta"
-- --     <$= jeta
-- --
-- --
-- -- jptH :: Foldl (Double, Jet) (Vars YodaFolder)
-- -- jptH = F.premap (sequenceA . fmap jpt) $ withVariations h
-- --   where
-- --     h :: Foldl (Double, Double) YodaFolder
-- --     h =
-- --       hist1DDef
-- --         (binD 0 50 500)
-- --         "$p_{\\mathrm T}$ [GeV]"
-- --         (dsigdXpbY pt gev)
-- --         "/pt"
-- --
-- -- jHs :: Foldl (Double, Jet) (Vars YodaFolder)
-- -- jHs = mappend jptH $ F.premap pure (withVariations jetaH)
