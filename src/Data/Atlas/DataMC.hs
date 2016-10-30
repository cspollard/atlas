{-# LANGUAGE TypeFamilies #-}

module Data.Atlas.DataMC where

import Control.Lens

data Data'
data MC

class HasMCInfo a where
    type MCInfo a :: *
    mcInfo :: Lens' a (MCInfo a)
