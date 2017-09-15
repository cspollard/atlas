{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module Atlas.ProcessInfo where

import           Data.Hashable
import           Data.Monoid    ((<>))
import           Data.Serialize
import           Data.Text      (Text)
import           GHC.Generics   (Generic)

data Simulation = FS | AFII | DS deriving (Eq, Ord, Show, Generic)

instance Serialize Simulation
instance Hashable Simulation

data ProcessInfo =
  ProcessInfo
  { dsid :: Int
  , sim  :: Simulation
  } deriving (Eq, Ord, Show, Generic)

instance Serialize ProcessInfo
instance Hashable ProcessInfo

hf :: Int -> Text
hf x =
  case x `mod` 3 of
    0 -> "L"
    1 -> "C"
    2 -> "B"


processTitle :: ProcessInfo -> Text
processTitle (ProcessInfo ds s) = dsToString <> simToString s
  where
    simToString AFII = " AFII"
    simToString _    = ""

    dsToString
      | ds == 0               = "data"
      | ds == 410501          = "PowPy8"
      | ds == 410511          = "PowPy8 radHi"
      | ds == 410512          = "PowPy8 radLo"
      | ds == 410225          = "aMCH7"
      | ds == 410525          = "PowH7"
      | ds == 410252          = "Sherpa"
      | ds == 410015          = "PowPy8 Wt"
      | ds == 410016          = "PowPy8 Wt"
      | ds == 410064          = "PowPy8 Wt DS"
      | ds == 410065          = "PowPy8 Wt DS"
      | ds == 410104          = "PowPy8 Wt radLo"
      | ds == 410106          = "PowPy8 Wt radLo"
      | ds == 410103          = "PowPy8 Wt radHi"
      | ds == 410105          = "PowPy8 Wt radHi"
      | ds == 410145          = "PowH++ Wt"
      | ds == 410146          = "PowH++ Wt"
      | btwn 364128 364141 ds = "Ztautau"
      | otherwise             = "other"

    btwn mn mx x = mn <= x && x <= mx
