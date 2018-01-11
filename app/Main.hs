{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Hashable (Hashable)
import Data.Text (Text, pack)
import Digraph (digraph)
import Flow
import GHC.Generics (Generic)
import Ingest (Ingester, ingest, ingester)
import System.FilePath -- TODO: list imports
import System.FilePath.Glob (compile)
import Text.Regex (Regex, matchRegex, matchRegexAll, mkRegex)

main :: IO ()
main = do
  graph <- ingest [testIngester] "."
  -- until we get fancy, let's make a graphviz graph out of these.
  putStrLn $ digraph graph

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

testIngester :: Ingester Node Edge
testIngester =
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
