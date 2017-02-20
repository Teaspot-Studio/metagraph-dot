module Data.Metagraph.Diagrams(
    metagraphDiagram
  ) where

import qualified Data.IntMap.Strict            as M
import           Data.Metagraph
import           Data.Metagraph.Internal.Types
import           Diagrams.Prelude

-- | Draw metagraph via diagrams
metagraphDiagram :: MetaGraph edge node -- ^ Metagraph to draw
  -> (edge -> Diagram b) -- ^ How to draw content of edge
  -> (node -> Diagram b) -- ^ How to draw content of node
  -> Diagram b
metagraphDiagram g drawEdge drawNode = undefined
