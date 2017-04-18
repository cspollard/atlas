{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Atlas.Histogramming where

import           Atlas.Corrected
import           Atlas.PhysObj
import           Atlas.Variation
import           Control.Applicative    (liftA2, liftA3)
import qualified Control.Foldl          as F
import           Control.Lens
import           Control.Monad          (join)
import           Data.Fillable
import           Data.Hist
import qualified Data.Histogram.Generic as G
import qualified Data.Map               as M
import           Data.Variation
import qualified Data.Vector            as V
import           Data.Weighted
import           Pipes
import qualified Pipes.Prelude          as P

-- fillFold :: Fillable b => b -> F.Fold (FillVec b, Weight b) b
-- fillFold f = F.Fold (flip $ uncurry filling) f id
--
-- filler
--   :: (Monad m, Fillable b)
--   => b -> Pipe (FillVec b, Weight b) b m ()
-- filler = F.purely P.scan . fillFold
--
--
fillFoldPO
  :: (Fillable c, Applicative f)
  => c -> F.Fold (f (Maybe (FillVec c, Weight c))) (f c)
fillFoldPO f = F.Fold (liftA2 g) (pure f) id
  where
    g h = maybe h (flip (uncurry filling) h)

fillerPO
  :: (Weight c ~ Double, Monad m, Fillable c)
  => c
  -> Producer (PhysObj (FillVec c)) m ()
  -> m (Vars c)
fillerPO h p = F.purely P.fold (fillFoldPO h) $ p >-> P.map runPhysObj


data Event = Event { mu :: Double, jets :: [Double] } deriving Show

eventsP :: Monad m => Producer Event m ()
eventsP = Pipes.each [e, e']
  where
    e = Event 1 [5]
    e' = Event 2 [7, 10]


-- jetsP :: Monad m => Pipe (PhysObj Event) (PhysObj Double) m ()
-- jetsP = P.mapFoldable f
--   where f e = fmap join . sequenceA $ jets <$> e


jetHs
  :: (Weight a ~ Double, FillVec a ~ Double, Monad m, Monoid a, Fillable a)
  => Producer (PhysObj Double) m () -> m (Vars (Histogram V.Vector BinD a))
jetHs p = h $ p >-> P.map (fmap dup)
  where dup x = (x, x)

muH
  :: (Weight a ~ Double, FillVec a ~ Double, Monad m, Monoid a, Fillable a)
  => Producer (PhysObj Double) m () -> m (Vars (Histogram V.Vector BinD a))
muH p = h $ p >-> P.map (fmap dup)
  where dup x = (x, x)


eventHs :: Monad m => Producer Event m () -> m [[Double]]
eventHs p = sequence
  [ P.toListM (p >-> P.mapFoldable jets)
  , P.toListM (p >-> P.map mu) ]

h
  :: (Weight a ~ Double, Fillable a, Monoid a, Monad m)
  => Producer (PhysObj (FillVec (Histogram V.Vector BinD a))) m ()
  -> m (Vars (Histogram V.Vector BinD a))
h = fillerPO (hEmpty $ binD 0 10 10)

hEmpty :: (Bin bin, Monoid a) => bin -> Histogram V.Vector bin a
hEmpty b =
  let v = V.replicate (nBins b) mempty
      uo = Just (mempty, mempty)
  in G.histogramUO b uo v

-- ptH :: Monad m => Pipe (Double, Double) (Hist1D BinD) m r
-- ptH = F.purely P.scan . hist1DFill . hEmpty $ binD 0 10 10
--
-- h :: (Monad m, Ord a) => Producer a m () -> m [Maybe a]
-- h = F.purely P.fold . sequenceA
--   $ [ F.last
--     , F.head
--     ]
--
-- -- evtHs :: Monad m => Producer (Double, Event) m () -> m (Maybe (Hist1D BinD))
-- evtHs p = P.last $ p >-> P.mapM (traverse jets) >-> P.concat >-> ptH
--

-- dsigdXpbY :: T.Text -> T.Text -> T.Text
-- dsigdXpbY x y =
--   "$\\frac{d\\sigma}{d" <> x <> "} \\frac{\\mathrm{pb}}{" <> y <> "}$"
--
-- mev, gev, rad, pt :: T.Text
-- mev = "\\mathrm{MeV}"
-- gev = "\\mathrm{GeV}"
-- rad = "\\mathrm{rad}"
-- pt = "p_{\\mathrm{T}}"
--
--
-- cut :: (a -> PhysObj Bool) -> PhysObj a -> PhysObj a
-- cut c o = do
--   p <- c =<< o
--   if p then o else lift (fail "fail cut")
--
-- channel :: (a -> PhysObj Bool) -> Fills a -> Fills a
-- channel f = F.premap (cut f)
--
-- channelWithLabel :: T.Text -> (a -> PhysObj Bool) -> Fills a -> Fills a
-- channelWithLabel n f = fmap (prefixF n) . channel f
--
-- channelsWithLabels :: [(T.Text, a -> PhysObj Bool)] -> Fills a -> Fills a
-- channelsWithLabels fns fills =
--   mconcat $ uncurry channelWithLabel <$> fns <*> pure fills
--
--
--
-- innerF :: Monad m => (a -> m b) -> Foldl (m b) c -> Foldl (m a) c
-- innerF g = F.premap (g =<<)
--
-- -- outerM :: Monad m => (a -> m b) -> Foldl (m b) (m c) -> Foldl (m a) (m c)
-- -- outerM g (F.Fold comb start done) = F.Fold comb' start' done'
-- --   where
-- --     start' = return start
-- --     done' x = done =<< x
-- --     comb' mx ma = flip comb (g =<< ma) <$> mx
-- --
-- -- outer :: Applicative m => (a -> m b) -> Foldl b c -> Foldl a (m c)
-- -- outer g = F.premap g . liftFA
--
-- bindF :: Monad m => (a -> m b) -> Foldl b (m c) -> Foldl a (m c)
-- bindF g (F.Fold comb start done) = F.Fold comb' start' done'
--   where
--     done' mx = done =<< mx
--     start' = pure start
--     comb' mx a = comb <$> mx <*> g a
--
-- liftFA :: Applicative m => Foldl b c -> Foldl (m b) (m c)
-- liftFA (F.Fold comb start done) = F.Fold comb' start' done'
--   where
--     start' = pure start
--     comb' xs x = comb <$> xs <*> x
--     done' = fmap done
--
--
-- -- TODO
-- -- I think this would work better with CPS
-- -- basically: I take a Cont (Double, Double) (Double, Double)
-- -- you can transform teh 2nd set of Doubles all you want,
-- -- as long as you deliver what I need
-- -- however, how do pass an Event to a Histo1D and a Prof1D
-- -- in the same "pipe"?
--
--
--
-- hist2DDef
--   :: (BinValue b ~ Double, IntervalBin b)
--   => b -> b -> T.Text -> T.Text -> Fill (Double, Double)
-- hist2DDef bx by xt yt =
--   fmap
--     ( Annotated [("XLabel", xt), ("YLabel", yt)]
--       . H2DD
--       . over bins (fmapBinX toArbBin)
--       . over bins (fmapBinY toArbBin)
--     )
--   <$> (pohelper .  hist2DFill . hEmpty $ Bin2D bx by)
--
-- prof1DDef
--   :: (BinValue b ~ Double, IntervalBin b)
--   => b -> T.Text -> T.Text -> Fill (Double, Double)
-- prof1DDef b xt yt =
--   fmap
--     ( Annotated [("XLabel", xt), ("YLabel", yt)]
--       . P1DD
--       . over bins toArbBin
--     )
--   <$> (pohelper . prof1DFill $ hEmpty b)
--
-- nH
--   :: Foldable f
--   => Int -> Fills (f a)
-- nH n =
--   singleton "/n"
--   <$> hist1DDef (binD 0 n (fromIntegral n)) "$n$" (dsigdXpbY "n" "1")
--   <$= fromIntegral . length
--
-- ptH :: HasLorentzVector a => Fills a
-- ptH =
--   singleton "/pt"
--   <$> hist1DDef
--       (logBinD 20 25 500) -- :: TransformedBin BinD (Log10BT Double))
--       "$p_{\\mathrm T}$ [GeV]"
--       (dsigdXpbY pt gev)
--   <$= view lvPt
--
-- etaH :: HasLorentzVector a => Fills a
-- etaH =
--   singleton "/eta"
--   <$> hist1DDef (binD (-3) 39 3) "$\\eta$" (dsigdXpbY "\\eta" "{\\mathrm rad}")
--   <$= view lvEta
--
-- lvHs
--   :: HasLorentzVector a => Fills a
-- lvHs = ptH `mappend` etaH
--
--
-- infixl 2 <$=
-- (<$=) :: Functor f => Foldl (f c) b -> (a -> c) -> Foldl (f a) b
-- h <$= f = F.premap (fmap f) h
--
-- matchRegex :: String -> String -> Bool
-- matchRegex rxp = matchTest (makeRegex rxp :: Regex)
--
-- filterFolder :: Maybe String -> Folder a -> Folder a
-- filterFolder s f = maybe f (`g` f) s
--   where
--     g s' =
--       let rxp = makeRegex s' :: Regex
--           h k _ = matchTest rxp $ T.unpack k
--       in inF (M.filterWithKey h)
