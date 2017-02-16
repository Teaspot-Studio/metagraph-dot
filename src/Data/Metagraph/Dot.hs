{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Metagraph.Dot(
    dotMetagraph
  , dotMetagraphText
  ) where

import           Data.GraphViz.Attributes.Complete
import           Data.GraphViz.Types.Generalised
import           Data.GraphViz.Types.Monadic
import qualified Data.IntMap.Strict                as M
import           Data.Metagraph
import           Data.Metagraph.Internal.Types
import           Data.Text.Lazy                    (Text, pack)

-- | Simplified 'dotMetagraph' for text metagraph
dotMetagraphText :: GraphID -- ^ Name of graphviz graph
  -> MetaGraph Text Text -- ^ Metagraph to convert
  -> DotGraph Text
dotMetagraphText gid = dotMetagraph gid
  (\MetaNode{..} -> (_nodePayload, Str _nodePayload, []))
  (\MetaEdge{..} -> (Str _edgePayload, []))
  ""

-- | Convert metagraph into Graphviz representation
dotMetagraph :: forall e n n' . GraphID -- ^ Name of graphviz graph
  -> (MetaNode e n -> (n', GraphID, Attributes)) -- ^ How to render node
  -> (MetaEdge e n -> (GraphID, Attributes)) -- ^ How to render edges
  -> n' -- ^ Dummy node for edges between meta edges
  -> MetaGraph e n -- ^ Metagraph to convert
  -> DotGraph n' -- ^ Resulting graphviz graph
dotMetagraph gid renderNode renderEdge dummy m = digraph gid $ dotMetagraph' m
  where
  dotMetagraph' :: MetaGraph e n -> Dot n'
  dotMetagraph' MetaGraph{..} = do
    node dummy [Style [SItem Invisible []]]
    mapM_ dotMetaNode  $ M.elems _metagraphNodes
    mapM_ dotMetaEdge' $ M.elems _metagraphEdges

  dotMetaNode :: MetaNode e n -> Dot n'
  dotMetaNode mn@MetaNode{..} = let (n', nid, attrs) = renderNode mn
    in case _nodeGraph of
      Nothing       -> node n' attrs
      Just subgraph -> cluster nid $ dotMetagraph' subgraph

  dotMetaEdge :: (MetaEdge e n -> (GraphID, Attributes))
    -> Attributes -> MetaEdge e n -> Dot n'
  dotMetaEdge renderEdge addAttrs me@MetaEdge{..} = do
    let (eid, attrs) = renderEdge me
        makeEndpoint mn endpos = let
          (e', eid, _) = renderNode mn
          in case _nodeGraph mn of
            Nothing -> (e', [])
            Just _  -> (dummy, [endpos . pack . show $ eid])
        (f, fatrs) = makeEndpoint _edgeFrom LTail
        (t, tatrs) = makeEndpoint _edgeTo LHead
    case _edgeGraph of
      Nothing -> edge f t (addAttrs ++ attrs ++ fatrs ++ tatrs)
      Just subgraph -> do
        cluster eid $ dotMetagraph' subgraph
        edge f dummy (addAttrs ++ attrs ++ fatrs ++ [LHead . pack . show $ eid])
        edge dummy t (addAttrs ++ attrs ++ tatrs ++ [LTail . pack . show $ eid])

  dotMetaEdge' :: MetaEdge e n -> Dot n'
  dotMetaEdge' e@MetaEdge{..} = dotMetaEdge renderEdge [Dir style] e
    where style = if isDirected _edgeDirected then Forward else NoDir
