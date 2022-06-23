{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE ViewPatterns   #-}
{-| Export PERT charts to various formats
-}
module Ernie.Export(
  DotNodeContent(..),
  dot,
  dotFile,
  algebraicGraph,
  defaultStyle
  ) where

import Algebra.Graph qualified as AG
import Algebra.Graph.Export.Dot (Attribute (..), Style (..))
import Algebra.Graph.Export.Dot qualified as Dot
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Ernie.Chart (TaskGraph (..), TaskID)
import Ernie.PERT (PERTEstimate (..))
import Ernie.Sample (Sample (..))
import Ernie.Task (Task (..))
import Ernie.Time (Days (..))

{-| A DOT representation of the dependency graph.
-}
dot :: DotNodeContent e => Text -> TaskGraph e -> Text
dot nm tg = Dot.export (defaultStyle tg nm) (algebraicGraph tg)

{-| Write the dependency graph to a DOT file
-}
dotFile :: DotNodeContent e => FilePath -> TaskGraph e -> IO ()
dotFile fp = TIO.writeFile fp . dot ""

{-| Convert the task graph to a 'Algebra.Graph.Graph' of task IDs
-}
algebraicGraph :: TaskGraph e -> AG.Graph TaskID
algebraicGraph TaskGraph{unTaskGraph} =
  let vs = Set.toList (Map.keysSet unTaskGraph)
      es = Map.toList unTaskGraph >>= \(a, (Set.toList -> bs, _)) -> (,a) <$> bs
  in AG.edges es <> AG.vertices vs

{-| Types that can be shown inside a DOT graph node
-}
class DotNodeContent e where
  getContent :: e -> Text

daysToText :: Days -> Text
daysToText (Days n) = Text.pack (take 4 $ show n) <> "d"

instance DotNodeContent Days where
  getContent = daysToText

instance DotNodeContent d => DotNodeContent (Sample d) where
  getContent (Sample s) = getContent s

instance DotNodeContent d => DotNodeContent (PERTEstimate d) where
  getContent PERTEstimate{pMin, pMode, pMax} =
    "{" <> getContent pMin <> "|" <> getContent pMode <> "|" <> getContent pMax <> "}"

instance DotNodeContent () where
  getContent () = ""

{-| Stack two graph contents vertically
-}
instance (DotNodeContent a, DotNodeContent b) => DotNodeContent (a, b) where
  getContent (a, b) = getContent a <> "|" <> getContent b

{-| Default style for exporting taks graphs to DOT files
-}
defaultStyle :: DotNodeContent e => TaskGraph e -> Text -> Style TaskID Text
defaultStyle TaskGraph{unTaskGraph} graphName =
  let tn i = maybe (Text.pack $ show i) (taskName . snd) (Map.lookup i unTaskGraph)
      cnt i = maybe "" (getContent . taskDuration . snd) (Map.lookup i unTaskGraph)
      content i =
        tn i <> "|" <> cnt i
      labels i = ["label" := content i]
  in Style
      { graphName
      , preamble =
          [ "node [shape=Mrecord]" -- This allows us to structure the records with | and {}
          , "rankdir=LR"
          ]
      , graphAttributes = []
      , defaultVertexAttributes =
          [ "color" := "darkgray"
          , "fillcolor" := "grey93"
          , "style" := "filled"
          , "height" := ".1"
          , "shape" := "record" -- set the shape again to avoid rounded corners
          ]
      , defaultEdgeAttributes = []
      , vertexName = Text.pack . show
      , vertexAttributes = labels
      , edgeAttributes = \_ _ -> []
      , attributeQuoting = Dot.DoubleQuotes
      }
