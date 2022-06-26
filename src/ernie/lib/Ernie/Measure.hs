{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns   #-}
{-| Measures on graphs
-}
module Ernie.Measure(
  measure,
  TaskMeasure(..),
  ClosedTaskID(..),
  -- * Computing measures on graphs
  criticalPaths,
  CritPath(..),
  Duration(..),
  -- * Etc.
  initialAndFinalTasks
  ) where

import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (maximumBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (..))
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.TDigest (TDigest)
import Data.TDigest qualified as TDigest
import Ernie.Chart (DependencyGraph (..), GraphStructure (..), TaskID (..),
                    structure)

data TaskMeasure =
  TaskMeasure
    { tmDuration      :: !(TDigest 25)
    , tmTotalDuration :: !(TDigest 25)
    , tmCritPathCount :: !(Sum Int)
    , tmTotalCount    :: !(Sum Int)
    }

instance Semigroup TaskMeasure where
  l <> r =
    TaskMeasure
      { tmDuration = tmDuration l <> tmDuration r
      , tmTotalDuration = tmTotalDuration l <> tmTotalDuration r
      , tmCritPathCount = tmCritPathCount l <> tmCritPathCount r
      , tmTotalCount = tmTotalCount l <> tmTotalCount r
      }

instance Monoid TaskMeasure where
  mempty = TaskMeasure mempty mempty mempty mempty

measure :: DependencyGraph TaskID Duration -> Map ClosedTaskID TaskMeasure
measure graph =
  let DependencyGraph (Map.map snd -> critPaths) = criticalPaths (initialAndFinalTasks graph)
      CritPath (Set.fromList -> critPathIDs) _ _ = fromMaybe (error "measure: FinalTask not found") (Map.lookup FinalTask critPaths)
      m taskID CritPath{cpTaskDuration = Duration dur, cpTaskEnd = Duration e} =
        TaskMeasure
          { tmDuration = TDigest.singleton dur
          , tmTotalDuration = TDigest.singleton e
          , tmCritPathCount = if taskID `Set.member` critPathIDs then 1 else 0
          , tmTotalCount = 1
          }
  in Map.mapWithKey m critPaths

{- Note: Computing Measures

Measures are distances between nodes in a dependency graph. We compute them
using the Floyd-Warshall algorithm, which requires us to convert the graph
to a 'FW.Matrix'.

The critical path is the longest path from beginning to end. However, if we
look at an arbitrary 'DependencyGraph' there is no guarantee that the graph
has exactly one connected component. (In fact there no guarantees about the
structure of the graph at all). That is why we add two special tasks when
converting the 'DependencyGraph' to a matrix: An initial task, depended on
by all tasks that don't currently depend on anything, and a final task,
depending on all tasks that currently nothing else depends on. Then we can
simply take the distance between initial and final tasks as the critical path.

Note that it is possible that the graph does not have any initial or final
tasks. This can be the case if there are circular dependencies (A depends on
B and B depends on A). If this happens then we cannot compute the critical
path.

-}

{-| Keeping track of the critical path
-}
data CritPath k n =
  CritPath
    { cpLongestPath  :: [k] -- ^ Longest (in terms of duration) path including this task
    , cpTaskEnd      :: !n
    , cpTaskDuration :: !n
    }

addTask :: Num n => k -> n -> CritPath k n -> CritPath k n
addTask t n (CritPath p d _) = CritPath (t:p) (d+n) n

single :: k -> n -> CritPath k n
single t n = CritPath [t] n n

criticalPaths :: (Num n, Ord n, Show k, Ord k, Show n) => DependencyGraph k n -> DependencyGraph k (CritPath k n)
criticalPaths DependencyGraph{unDependencyGraph} =
  let lkp k = fromMaybe (error $ show k <> " " <> show unDependencyGraph) (Map.lookup k result)
      f k (deps, n) =
        case Set.toList deps of
          [] -> (deps, single k n)
          xs -> (deps, addTask k n $ maximumBy (comparing cpTaskEnd) (snd . lkp <$> xs))
      result = Map.mapWithKey f unDependencyGraph
  in DependencyGraph result

data ClosedTaskID =
  FinalTask
  | SomeTask TaskID
  deriving stock (Eq, Ord, Show)

initialAndFinalTasks :: Monoid n => DependencyGraph TaskID n -> DependencyGraph ClosedTaskID n
initialAndFinalTasks graph =
  let DependencyGraph{unDependencyGraph} = graph
      GraphStructure{gsFinalTasks} = structure graph
      old = (first SomeTask . second (first (Set.map SomeTask))) <$> Map.toList unDependencyGraph
      finalLinks = [(FinalTask, (Set.map SomeTask gsFinalTasks, mempty))]
  in DependencyGraph
      $ Map.fromListWith (<>)
      $ old
        ++ finalLinks

newtype Duration = Duration Double
  deriving newtype (Num, Fractional, Show, Ord, Eq)
  deriving (Semigroup, Monoid) via (Sum Double)
