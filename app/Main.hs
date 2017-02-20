{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.GraphViz
import           Data.GraphViz.Printing hiding ((<>))
import           Data.Metagraph
import           Data.Metagraph.Dot
import           Data.Monoid
import           Data.Text              (pack)
import qualified Data.Text.IO           as T
import qualified Data.Text.Lazy         as T
import           Prelude                as P
import           Turtle

main :: IO ()
main = do
  renderMetaGraph testGraph1 "graph1"
  renderMetaGraph testGraph2 "graph2"
  renderMetaGraph testGraph3 "graph3"
  renderMetaGraph appleGraph "apple"

-- | Render a ps to given path
renderMetaGraph :: MetaGraph T.Text T.Text -> P.FilePath -> IO ()
renderMetaGraph gr path = do
  putStrLn $ "Printing " ++ path ++ " ..."
  let
    dotPath = path ++ ".dot"
    psPath  = path ++ ".ps"
  T.writeFile dotPath $ T.toStrict $ printIt $ dotMetagraphText (Str "") gr
  _ <- shell ("dot -Tps " <> pack dotPath <> " -o " <> pack psPath) Turtle.empty
  return ()

-- | Simple planar graph
testGraph1 :: MetaGraph T.Text T.Text
testGraph1 = buildMetaGraph $ do
  g <- newMetaGraph
  a <- newNode g Nothing "A"
  b <- newNode g Nothing "B"
  _ <- newEdge g Directed a b Nothing "1"
  _ <- newEdge g Directed b a Nothing "2"
  return g

-- | Non-planar metagraph
testGraph2 :: MetaGraph T.Text T.Text
testGraph2 = buildMetaGraph $ do
  g  <- newMetaGraph
  gA <- newMetaGraph
  a1 <- newNode gA Nothing "A1"
  a2 <- newNode gA Nothing "A2"
  a3 <- newNode gA Nothing "A3"
  _ <- newEdge gA Directed a1 a2 Nothing "a12"
  _ <- newEdge gA Directed a2 a3 Nothing "a23"
  _ <- newEdge gA Directed a3 a1 Nothing "a31"
  a  <- newNode g (Just gA) "A"
  b  <- newNode g Nothing "B"
  _ <- newEdge g Directed b a1 Nothing "ba1"
  return g

-- | Metagraph with metaedge
testGraph3 :: MetaGraph T.Text T.Text
testGraph3 = buildMetaGraph $ do
  g  <- newMetaGraph
  gE <- newMetaGraph
  e1 <- newNode gE Nothing "E1"
  e2 <- newNode gE Nothing "E2"
  e3 <- newNode gE Nothing "E3"
  _ <- newEdge gE Directed e1 e2 Nothing "E12"
  _ <- newEdge gE Directed e2 e3 Nothing "E23"
  _ <- newEdge gE Directed e3 e1 Nothing "E31"
  a  <- newNode g Nothing "A"
  b  <- newNode g Nothing "B"
  _ <- newEdge g Directed a b (Just gE) "E"
  return g

-- | Metagraph of relations in the text:
--
-- @
-- An apple is a vegetable, that has a spherical shape and one of the following
-- colors: green, yellow and red, but not a blue one.
-- @
appleGraph :: MetaGraph T.Text T.Text
appleGraph = buildMetaGraph $ do
  g <- newMetaGraph
  apple <- newNode g Nothing "Apple"
  vegetable <- newNode g Nothing "Vegetable"
  _ <- newEdge g Directed apple vegetable Nothing "is"
  shape <- newNode g Nothing "Shape"
  sg <- newMetaGraph
  sphere <- newNode g Nothing "Sphere"
  _ <- newEdge g Directed apple shape (Just sg) "has like"
  cg <- newMetaGraph
  green <- newNode cg Nothing "Green"
  yellow <- newNode cg Nothing "Yellow"
  red <- newNode cg Nothing "Red"
  blue <- newNode cg Nothing "Blue"
  color <- newNode g (Just cg) "Color"
  acg <- newMetaGraph
  addNode acg green
  addNode acg yellow
  addNode acg red
  appleColor <- newNode g (Just acg) "Apple color"
  _ <- newEdge g Directed apple appleColor Nothing "has"
  return g
