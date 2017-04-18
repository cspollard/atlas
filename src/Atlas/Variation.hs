module Atlas.Variation
  ( module X
  , Vars
  , variationsToMap
  ) where

import           Control.Lens
import qualified Data.Map       as M
import qualified Data.Text      as T
import           Data.Variation as X

type Vars = Variations (M.Map T.Text)

variationsToMap :: Ord k => k -> Variations (M.Map k) a -> M.Map k a
variationsToMap k vs = view variations vs & at k ?~ view nominal vs
