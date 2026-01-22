{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | Export PERT charts to various formats
module Ernie.Export (
    DotNodeContent (..),
    dot,
    dotFile,
) where

import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.GraphViz.Attributes (arrowFrom, arrowTo, bgColor, color, filled, noArrow, rounded, style, textLabel)
import Data.GraphViz.Attributes.Colors.X11 qualified as Colors
import Data.GraphViz.Attributes.Complete qualified as A
import Data.GraphViz.Types qualified as GVT
import Data.GraphViz.Types.Generalised (DotGraph (..))
import Data.GraphViz.Types.Monadic qualified as GV
import Data.Map qualified as Map
import Data.Monoid (Sum (..))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.TDigest qualified as TDigest
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Ernie.Chart (DependencyGraph (..), TaskID (..))
import Ernie.Measure (TaskMeasure (..))
import Ernie.PERT (PERTEstimate (..))
import Ernie.Sample (Sample (..))
import Ernie.Task (TType (..), Task (..))
import Ernie.Time (Days (..))

-- | A DOT representation of the dependency graph.
dot :: (DotNodeContent e) => Text -> DependencyGraph TaskID (Task e) -> Text
dot nm = TL.toStrict . GVT.printDotGraph . dot' nm

groups :: DependencyGraph TaskID (Task e) -> Map.Map (Maybe Text) (Seq.Seq (TaskID, Set.Set TaskID, Task e))
groups =
    let mkGroup (taskID, (deps, task@Task{taskGroup})) = (taskGroup, Seq.singleton (taskID, deps, task))
     in Map.fromListWith (<>) . fmap mkGroup . Map.toList . unDependencyGraph

insertGroup :: (DotNodeContent e) => Maybe Text -> Seq.Seq (TaskID, Set.Set TaskID, Task e) -> GV.DotM TaskID ()
insertGroup groupName xs = do
    let withGroup m = case groupName of
            Nothing -> m
            Just t -> GV.cluster (GV.Str $ TL.fromStrict t) $ do
                GV.graphAttrs
                    [ textLabel $ TL.fromStrict t
                    , style $ A.SItem A.Dashed []
                    , A.LabelJust A.JLeft
                    ]
                m
        allTasksInGroup = foldMap (\(i, _, _) -> Set.singleton i) xs

    withGroup $ flip traverse_ xs $ \(i, deps, Task{taskName, taskDuration, taskType}) ->
        case taskType of
            TEvent -> do
                GV.node i [A.Shape A.DiamondShape, A.Label $ A.RecordLabel [A.FieldLabel " "]]
                GV.node
                    (negate i)
                    [ A.Label $ A.RecordLabel $ A.FieldLabel (TL.fromStrict taskName) : getContent taskDuration
                    , bgColor Colors.LightGray
                    , style rounded
                    ]
                GV.edge
                    i
                    (negate i)
                    [ color Colors.LightGray
                    , A.Weight (GVT.Int 10)
                    , style $ A.SItem A.Dashed []
                    , arrowTo noArrow
                    , arrowFrom noArrow
                    ]
                flip traverse_ deps $ \source -> do
                    when (source `Set.member` allTasksInGroup) (GV.edge source i [])
            TTask -> do
                let lbl = A.Label $ A.RecordLabel $ A.FieldLabel (TL.fromStrict taskName) : getContent taskDuration
                GV.node i [lbl]
                flip traverse_ deps $ \source -> do
                    when (source `Set.member` allTasksInGroup) (GV.edge source i [])
    flip traverse_ xs $ \(i, deps, _) -> do
        flip traverse_ deps $ \source -> do
            when (not (source `Set.member` allTasksInGroup)) (GV.edge source i [])

dot' :: (DotNodeContent e) => Text -> DependencyGraph TaskID (Task e) -> DotGraph TaskID
dot' (TL.fromStrict -> nm) g = GV.digraph (GV.Str nm) $ do
    GV.graphAttrs [A.RankDir A.FromLeft]
    GV.nodeAttrs
        [ A.Shape A.Record
        , style filled
        , bgColor Colors.Gray93
        , A.Height 0.1
        ]
    traverse_ (uncurry insertGroup) (Map.toList $ groups g)

-- | Write the dependency graph to a DOT file
dotFile :: (DotNodeContent e) => FilePath -> DependencyGraph TaskID (Task e) -> IO ()
dotFile fp = TIO.writeFile fp . dot ""

-- | Types that can be shown inside a DOT graph node
class DotNodeContent e where
    getContent :: e -> [A.RecordField]

daysToText :: Days -> Text
daysToText (Days n) = Text.pack (take 4 $ show n) <> "d"

instance DotNodeContent Days where
    getContent = return . A.FieldLabel . TL.fromStrict . daysToText

instance (DotNodeContent d) => DotNodeContent (Sample d) where
    getContent (Sample s) = getContent s

instance (DotNodeContent d) => DotNodeContent (Maybe d) where
    getContent = maybe [] getContent

instance (DotNodeContent d) => DotNodeContent (PERTEstimate d) where
    getContent PERTEstimate{pMin, pMode, pMax} =
        [ A.FlipFields
            (getContent pMin <> getContent pMode <> getContent pMax)
        ]

instance DotNodeContent () where
    getContent () = []

instance DotNodeContent TaskMeasure where
    getContent TaskMeasure{tmCritPathCount = Sum c, tmTotalCount = Sum t, tmTotalDuration} =
        let perc = (fromIntegral c / fromIntegral t) * (100 :: Double)
            cp = A.FieldLabel $ TL.fromStrict $ Text.pack $ "Crit. Path: " <> (take 4 $ show perc) <> "%"
            five = TDigest.quantile 0.05 tmTotalDuration
            fifty = TDigest.quantile 0.5 tmTotalDuration
            ninetyFive = TDigest.quantile 0.95 tmTotalDuration
            sw = A.FieldLabel . TL.fromStrict . maybe "" (Text.pack . take 4 . show)
            dur = A.FlipFields [sw five, sw fifty, sw ninetyFive]
         in [cp, dur]

-- | Stack two graph contents vertically
instance (DotNodeContent a, DotNodeContent b) => DotNodeContent (a, b) where
    getContent (a, b) = getContent a <> getContent b

-- | Default style for exporting taks graphs to DOT files

-- defaultStyle :: DotNodeContent e => DependencyGraph TaskID (Task e) -> Text -> Style TaskID Text
-- defaultStyle DependencyGraph{unDependencyGraph} graphName =
--   let tn i = maybe (Text.pack $ show i) (taskName . snd) (Map.lookup i unDependencyGraph)
--       cnt i = maybe "" (getContent . taskDuration . snd) (Map.lookup i unDependencyGraph)
--       content i =
--         tn i <> "|" <> cnt i
--       labels i = ["label" := content i]
--   in Style
--       { graphName
--       , preamble =
--           [ "node [shape=Mrecord]" -- This allows us to structure the records with | and {}
--           , "rankdir=LR"
--           ]
--       , graphAttributes = []
--       , defaultVertexAttributes =
--           [ "color" := "darkgray"
--           , "fillcolor" := "grey93"
--           , "style" := "filled"
--           , "height" := ".1"
--           , "shape" := "record" -- set the shape again to avoid rounded corners
--           ]
--       , defaultEdgeAttributes = []
--       , vertexName = Text.pack . show
--       , vertexAttributes = labels
--       , edgeAttributes = \_ _ -> []
--       , attributeQuoting = Dot.DoubleQuotes
--       }
