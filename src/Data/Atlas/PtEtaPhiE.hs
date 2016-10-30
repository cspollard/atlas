module Data.Atlas.PtEtaPhiE ( lvsFromTTree, PtEtaPhiEs(..) ) where

import GHC.Float
import Control.Applicative (ZipList(..))
import Data.HEP.LorentzVector
import Data.TTree

newtype PtEtaPhiEs = PtEtaPhiEs [PtEtaPhiE]

lvsFromTTree :: MonadIO m => String -> String -> String -> String -> TR m PtEtaPhiEs
lvsFromTTree ptn etan phin en = do pts <- fmap float2Double <$> readBranch ptn
                                   etas <- fmap float2Double <$> readBranch etan
                                   phis <- fmap float2Double <$> readBranch phin
                                   es <- fmap float2Double <$> readBranch en

                                   let tlvs = PtEtaPhiE <$> pts <*> etas <*> phis <*> es
                                   return . PtEtaPhiEs . getZipList $ tlvs
