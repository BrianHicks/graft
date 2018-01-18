module Main where

import Digraph (digraph)
import Ingest (ingest)
import Language.Haskell (haskell)
import Protolude

main :: IO ()
main = do
  graph <- ingest [haskell] "."
  -- until we get fancy, let's make a graphviz graph out of these.
  putStrLn (digraph graph)
