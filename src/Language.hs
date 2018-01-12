module Language
  ( Node
  , Edge
  , haskell
  ) where

import Data.Hashable (Hashable)
import Data.Text (Text, pack)
import Flow
import GHC.Generics (Generic)
import Ingest (Ingester, ingester)
import System.FilePath.Glob (compile)
import Text.Regex (Regex, matchRegex, matchRegexAll, mkRegex)

data Node
  = Haskell Text
  | Filetype Text
  deriving (Show, Generic)

instance Hashable Node

data Edge
  = Imports
  | HasFiletype
  deriving (Show, Generic)

instance Hashable Edge

haskell :: Ingester Node Edge
haskell =
  let interestingFiles = compile "**/*.hs"
      haskellModule = mkRegex "^module ([a-zA-Z\\.]+).*$"
      haskellImport = mkRegex "^import ([a-zA-Z\\.]+)( \\(.+\\))?$"
      parse filepath = do
        contents <- readFile filepath
        let name :: Node
            name =
              Haskell <|
              case matchRegex haskellModule contents of
                Just [moduleName] -> pack moduleName
                _ -> pack filepath
        let imports =
              matchAll haskellImport contents |>
              map
                (\items ->
                   case items of
                     i:_ -> [(Imports, Haskell <| pack i)]
                     [] -> []) |>
              concat
        pure (name, (HasFiletype, Filetype "haskell") : imports)
  in ingester interestingFiles parse

matchAll :: Regex -> String -> [[String]]
matchAll = matchAllAcc []

matchAllAcc :: [[String]] -> Regex -> String -> [[String]]
matchAllAcc acc regex contents =
  case matchRegexAll regex contents of
    Just (_, _, next, match) -> matchAllAcc (match : acc) regex next
    Nothing -> reverse acc
