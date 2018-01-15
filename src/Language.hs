module Language
  ( Node(..)
  , Edge(..)
  ) where

import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Language.Haskell as Haskell

-- TODO: surely there's some way to move these into a less shared
-- location, while still sharing things like `Imports` between all the
-- languages?
data Node
  = HaskellNode Haskell.Node
  deriving (Show, Generic)

instance Hashable Node

data Edge
  = HaskellEdge Haskell.Edge
  deriving (Show, Generic)

instance Hashable Edge
