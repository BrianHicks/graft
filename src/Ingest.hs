module Ingest where

import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Text (Text, pack)
import Flow
import System.Directory.PathWalk (pathWalk)
import System.FilePath (FilePath)
import System.FilePath.Glob (Pattern, compile, match)

data Ingester = Ingester
  { forFiles :: Pattern
  , nodeName :: FilePath -> IO Text
  , relations :: FilePath -> IO [(Text, Text)]
  }

ingester ::
     Pattern
  -> (FilePath -> IO Text)
  -> (FilePath -> IO [(Text, Text)])
  -> Ingester
ingester = Ingester

-- ingest :: [Ingester] -> FilePath -> IO (Gr Text Text)
ingest :: [Ingester] -> FilePath -> IO ()
ingest patterns root
  -- 1. get all the files in the root
 = do
  pathWalk root <| \dir subdirs files -> putStrLn dir
  -- 2. map over the ingesters...
  --    is Gr a monoid? That'd make things easier.
  -- 2a. getting the node names and relations for those ingesters whose `forFiles` match
  -- 3. convert that information to something I can make into a graph
  -- 4. return the graph, hooray!
  pure ()

testIngester :: Ingester
testIngester =
  ingester (compile "*.hs") (\path -> pure <| pack path) (\path -> pure [])
