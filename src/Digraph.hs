module Digraph
  ( digraph
  ) where

import Data.Graph.Inductive.Graph (labEdges, labNodes)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (intersperse)
import Flow

digraph :: (Show node, Show edge) => Gr node edge -> String
digraph graph =
  let nodes =
        labNodes graph |>
        map (\(id, label) -> quoted id ++ "[label=" ++ quoted label ++ "];")
      edges =
        labEdges graph |>
        map
          (\(subj, obj, label) ->
             quoted subj ++
             " -> " ++ quoted obj ++ "[label=" ++ quoted label ++ "];")
  in "digraph {\n" ++ join "\n" nodes ++ "\n" ++ join "\n" edges ++ "\n}"

join :: String -> [String] -> String
join char items = items |> intersperse char |> concat

quoted :: Show a => a -> String
quoted a = "\"" ++ filter (/= '"') (show a) ++ "\"" -- TODO: less cheating
