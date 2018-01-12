module Main where

import Digraph (digraph)
import Flow
import Ingest (ingest)
import Language.Haskell (haskell)

main :: IO ()
main = do
  graph <- ingest [haskell] "."
  -- until we get fancy, let's make a graphviz graph out of these.
  putStrLn <| digraph graph
