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
import Data.Monoid

-- | Simplified 'dotMetagraph' for text metagraph
dotMetagraphText :: GraphID -- ^ Name of graphviz graph
  -> MetaGraph Text Text -- ^ Metagraph to convert
  -> DotGraph Text
dotMetagraphText gid = dotMetagraph gid
  (\MetaNode{..} -> _nodePayload)
  (\MetaEdge{..} -> _edgePayload)
  (\MetaNode{..} -> (_nodePayload, Str _nodePayload, []))
  (\MetaEdge{..} -> (Str _edgePayload, [Label $ StrLabel _edgePayload]))
  (pack . show)

-- | Convert metagraph into Graphviz representation
dotMetagraph :: forall e n n' . GraphID -- ^ Name of graphviz graph
  -> (MetaNode e n -> Text) -- ^ Make label for metanode
  -> (MetaEdge e n -> Text) -- ^ Make label for metaedge
  -> (MetaNode e n -> (n', GraphID, Attributes)) -- ^ How to render node
  -> (MetaEdge e n -> (GraphID, Attributes)) -- ^ How to render edges
  -> (MetaGraphId -> n') -- ^ Render of metagraph top level (should be uniq per subgraph), invisible
  -> MetaGraph e n -- ^ Metagraph to convert
  -> DotGraph n' -- ^ Resulting graphviz graph
dotMetagraph gid mkNodeLabel mkEdgeLabel renderNode renderEdge subgraphNode m = digraph gid $ dotMetagraph' m
  where
  dotMetagraph' :: MetaGraph e n -> Dot n'
  dotMetagraph' MetaGraph{..} = do
    node (subgraphNode _metagraphId) [Style [SItem Invisible []], FixedSize SetNodeSize, Width 0.1]
    mapM_ dotMetaNode  $ M.elems _metagraphNodes
    mapM_ dotMetaEdge' $ M.elems _metagraphEdges

  dotMetaNode :: MetaNode e n -> Dot n'
  dotMetaNode mn@MetaNode{..} = let (n', nid, attrs) = renderNode mn
    in case _nodeGraph of
      Nothing       -> node n' attrs
      Just subgraph -> cluster nid $ do
        graphAttrs [Label $ StrLabel $ mkNodeLabel mn]
        dotMetagraph' subgraph

  dotMetaEdge :: (MetaEdge e n -> (GraphID, Attributes))
    -> Attributes -> MetaEdge e n -> Dot n'
  dotMetaEdge renderEdge addAttrs me@MetaEdge{..} = do
    let (eid, attrs) = renderEdge me
        makeEndpoint mn endpos = let
          (e', eid, _) = renderNode mn
          in case _nodeGraph mn of
            Nothing -> (e', [])
            Just g  -> (subgraphNode $ _metagraphId g, [endpos . pack . show $ eid])
        (f, fatrs) = makeEndpoint _edgeFrom LTail
        (t, tatrs) = makeEndpoint _edgeTo LHead
    case _edgeGraph of
      Nothing -> edge f t (addAttrs ++ attrs ++ fatrs ++ tatrs)
      Just subgraph -> do
        cluster eid $ do
          graphAttrs [Label $ StrLabel $ mkEdgeLabel me]
          dotMetagraph' subgraph
        let dummy = subgraphNode $ _metagraphId subgraph
        edge f dummy (addAttrs ++ attrs ++ fatrs ++ [LHead . pack . show $ eid])
        edge dummy t (addAttrs ++ attrs ++ tatrs ++ [LTail . pack . show $ eid])

  dotMetaEdge' :: MetaEdge e n -> Dot n'
  dotMetaEdge' e@MetaEdge{..} = dotMetaEdge renderEdge [Dir style] e
    where style = if isDirected _edgeDirected then Forward else NoDir
