module Digraph
  ( digraph
  ) where

import Data.Graph.Inductive.Graph (labEdges, labNodes)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Text (append, filter, intercalate)
import Flow
import Protolude

-- TODO: dang, this is a lot harder with Text. There must be some templating
-- library that would make this nicer.
digraph :: (Show node, Show edge) => Gr node edge -> Text
digraph graph =
  let nodes =
        map
          (\(id, label) ->
             quoted id `append` "[label=" `append` quoted label `append` "];")
          (labNodes graph)
      edges =
        map
          (\(subj, obj, label) ->
             quoted subj `append` " -> " `append` quoted obj `append` "[label=" `append`
             quoted label `append`
             "];")
          (labEdges graph)
  in "digraph {\n" `append` Data.Text.intercalate "\n" nodes `append` "\n" `append`
     Data.Text.intercalate "\n" edges `append`
     "\n}"

quoted :: Show a => a -> Text
quoted a = "\"" `append` Data.Text.filter (/= '"') (show a) `append` "\"" -- TODO: less cheating
