module Atlas.Variation
  ( module X
  , Vars
  ) where

import qualified Data.Map       as M
import qualified Data.Text      as T
import           Data.Variation as X

type Vars = Variations (M.Map T.Text)
