{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Data.Atlas.Histogramming
  ( module X
  , dsigdXpbY
  , mev, gev, rad, pt
  ) where

import qualified Control.Foldl  as F
import           Control.Lens
import           Data.Foldable  (toList)
import           Data.Maybe     (listToMaybe)
import           Data.Semigroup
import           Data.Text      (Text)

import           Data.Hist      as X


dsigdXpbY :: Text -> Text -> Text
dsigdXpbY x y = "$\\frac{d\\sigma}{d" <> x <> "} \\frac{\\mathrm{pb}}{" <> y <> "}$"

mev, gev, rad, pt :: Text
mev = "\\mathrm{MeV}"
gev = "\\mathrm{GeV}"
rad = "\\mathrm{rad}"
pt = "p_{\\mathrm{T}}"


-- yodaHist :: Int -> Double -> Double -> Text -> Text -> YodaObj
-- yodaHist nb xmin xmax xl yl =
--     yodaHist1D nb xmin xmax
--         & annots . at "XLabel" ?~ xl
--         & annots . at "YLabel" ?~ yl

-- yodaProf :: Int -> Double -> Double -> Text -> Text -> YodaObj
-- yodaProf nb xmin xmax xl yl =
--     yodaProf1D nb xmin xmax
--         & annots . at "XLabel" ?~ xl
--         & annots . at "YLabel" ?~ yl
