module Data.Atlas.PtEtaPhiE ( lvsFromTTreeD, lvsFromTTreeF ) where

import           Control.Applicative    (ZipList (..))
import           Data.HEP.LorentzVector
import           Data.TTree
import           GHC.Float

lvsFromTTreeD :: MonadIO m => String -> String -> String -> String -> TR m (ZipList PtEtaPhiE)
lvsFromTTreeD ptn etan phin en = do
  pts <- readBranch ptn
  etas <- readBranch etan
  phis <- readBranch phin
  es <- readBranch en

  return $ PtEtaPhiE <$> pts <*> etas <*> phis <*> es


lvsFromTTreeF :: MonadIO m => String -> String -> String -> String -> TR m (ZipList PtEtaPhiE)
lvsFromTTreeF ptn etan phin en = do
  pts <- fmap float2Double <$> readBranch ptn
  etas <- fmap float2Double <$> readBranch etan
  phis <- fmap float2Double <$> readBranch phin
  es <- fmap float2Double <$> readBranch en

  return $ PtEtaPhiE <$> pts <*> etas <*> phis <*> es
