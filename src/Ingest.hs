module Ingest
  ( Ingester
  , ingester
  , ingest
  ) where

import Data.Graph.Inductive.Graph
       (LEdge, LNode, empty, insEdges, insNodes, labEdges, labNodes,
        mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Hashable (Hashable, hash)
import Data.Monoid (Monoid, mappend, mempty)
import Data.Text (Text, pack)
import Flow
import Protolude
import System.Directory.PathWalk (pathWalkAccumulate)
import System.FilePath (FilePath, combine, takeExtension)
import System.FilePath.Glob (Pattern, match)

type FileInfo node edge = (node, [(edge, node)])

data Ingester node edge = Ingester
  { forFiles :: Pattern
  , parse :: FilePath -> IO (FileInfo node edge)
  }

-- I don't feel great about this, but it seems to be the best way to
-- get a Monoid instance for Graph... I don't think it can be in the
-- inductive graph library proper since this isn't valid for all kinds
-- of graphs, but for ours it is so... ok?
newtype Graph node edge =
  Graph (Gr node edge)

instance Monoid (Graph node edge) where
  mempty = Graph Data.Graph.Inductive.Graph.empty
  mappend (Graph x) (Graph y)
    -- note that the order matters here... nodes must come before
    -- edges since the graph library rightly refuses to add dangling
    -- eges.
   = x |> insNodes (labNodes y) |> insEdges (labEdges y) |> Graph

unwrap :: Graph node edge -> Gr node edge
unwrap (Graph g) = g

ingester ::
     Pattern -> (FilePath -> IO (FileInfo node edge)) -> Ingester node edge
ingester = Ingester

ingest ::
     (Hashable node, Hashable edge)
  => [Ingester node edge]
  -> FilePath
  -> IO (Gr node edge)
ingest ingesters root = do
  accumulated <- pathWalkAccumulate root (ingestDirectory ingesters)
  pure (unwrap accumulated)

-- this signature is weird because it's meant to be used by pathWalk
ingestDirectory ::
     (Hashable node, Hashable edge)
  => [Ingester node edge]
  -> FilePath
  -> [FilePath]
  -> [FilePath]
  -> IO (Graph node edge)
ingestDirectory ingesters dir _ files = do
  let fullPaths = map (combine dir) files
  -- TODO: strict evaluation for FS operations
  matched <- mapM (ingestFiles fullPaths) ingesters
  pure $
    case flatten matched of
      ([], []) -> mempty
      (nodes, edges) -> Graph (mkGraph nodes edges)

ingestFiles ::
     (Hashable node, Hashable edge)
  => [FilePath]
  -> Ingester node edge
  -> IO ([LNode node], [LEdge edge])
ingestFiles files ingester = do
  let interesting = filter (match (forFiles ingester)) files
  nodesAndEdges <- mapM (ingestFile ingester) interesting
  pure (flatten nodesAndEdges)

ingestFile ::
     (Hashable node, Hashable edge)
  => Ingester node edge
  -> FilePath
  -> IO ([LNode node], [LEdge edge])
ingestFile (Ingester _ getInfo) filepath = do
  (subject, relations) <- getInfo filepath
  -- edges
  let nodes = toNode subject : map (\(_, obj) -> toNode obj) relations
  let edges = map (\(pred, obj) -> toEdge subject pred obj) relations
  pure (nodes, edges)

toNode :: Hashable node => node -> LNode node
toNode label = (hash label, label)

toEdge :: (Hashable node, Hashable edge) => node -> edge -> node -> LEdge edge
toEdge subject predicate object = (hash subject, hash object, predicate)

flatten :: [([LNode node], [LEdge edge])] -> ([LNode node], [LEdge edge])
flatten everything =
  foldr
    (\(newNodes, newEdges) (nodes, edges) ->
       (newNodes ++ nodes, newEdges ++ edges))
    ([], [])
    everything
