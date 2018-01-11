module Ingest
  ( Ingester
  , ingester
  , ingest
  ) where

import Data.Graph.Inductive.Graph
       (LEdge, LNode, mkGraph, prettyPrint)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Hashable (hash)
import Data.Text (Text, pack)
import Flow
import System.Directory.PathWalk (pathWalk)
import System.FilePath (FilePath, combine, takeExtension)
import System.FilePath.Glob (Pattern, match)

-- | this is also Object, but too many type aliases :S
type Subject = Text

type Predicate = Text

type FileInfo = (Subject, [(Predicate, Subject)])

data Ingester = Ingester
  { forFiles :: Pattern
  , parse :: FilePath -> IO FileInfo
  }

instance Show Ingester where
  show (Ingester forFiles _) = "Ingester for (" ++ show forFiles ++ ")"

ingester :: Pattern -> (FilePath -> IO FileInfo) -> Ingester
ingester = Ingester

-- ingest :: [Ingester] -> FilePath -> IO (Gr Subject Predicate)
ingest :: [Ingester] -> FilePath -> IO ()
ingest ingesters root = do
  pathWalk root <| \dir subdirs files -> do
    let fullPaths = map (combine dir) files
    matched <- ingesters |> mapM (ingestFiles fullPaths)
    case flattenNodesAndEdges matched of
      ([], []) -> pure ()
      (nodes, edges) ->
        (mkGraph nodes edges :: Gr Subject Predicate) |> prettyPrint
  -- 2a. getting the node names and relations for those ingesters whose `forFiles` match
  -- 3. convert that information to something I can make into a graph
  -- 4. return the graph, hooray!
  pure ()

toNode :: Subject -> LNode Subject
toNode label = (hash label, label)

toEdge :: Subject -> Predicate -> Subject -> LEdge Predicate
toEdge subject predicate object = (hash subject, hash object, predicate)

flattenNodesAndEdges ::
     [([LNode Subject], [LEdge Predicate])]
  -> ([LNode Subject], [LEdge Predicate])
flattenNodesAndEdges everything =
  foldr
    (\(newNodes, newEdges) (nodes, edges) ->
       (newNodes ++ nodes, newEdges ++ edges))
    ([], [])
    everything

ingestFiles :: [FilePath] -> Ingester -> IO ([LNode Subject], [LEdge Predicate])
ingestFiles files ingester = do
  let interesting = filter (match (forFiles ingester)) files
  nodesAndEdges <- mapM (ingestFile ingester) interesting
  pure <| flattenNodesAndEdges nodesAndEdges

ingestFile :: Ingester -> FilePath -> IO ([LNode Subject], [LEdge Predicate])
ingestFile (Ingester _ getInfo) filepath = do
  (subject, relations) <- getInfo filepath
  -- edges
  let out =
        relations ++
        [ ("extension", filepath |> takeExtension |> drop 1 |> pack)
        , ("path", pack filepath)
        ] -- TODO: treat these as a set and don't override if predicate already set
  let nodes = toNode subject : map (\(_, obj) -> toNode obj) out
  let edges = map (\(pred, obj) -> toEdge subject pred obj) out
  pure (nodes, edges)
