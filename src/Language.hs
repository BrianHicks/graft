module Language
  ( Node(..)
  , Edge(..)
  ) where

import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)

-- TODO: surely there's some way to move these into a less shared
-- location, while still sharing things like `Imports` between all the
-- languages?
data Node
  = Haskell Text [Text]
  | Filetype Text
  deriving (Show, Generic)

instance Hashable Node

data Edge
  = Imports
  | HasFiletype
  deriving (Show, Generic)

instance Hashable Edge
