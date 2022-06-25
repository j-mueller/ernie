{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE NamedFieldPuns #-}
{-| Measures on graphs
-}
module Ernie.Measure(
  measure,
  TaskMeasure(..),
  -- * Computing measures on graphs
  criticalPath,
  Duration(..),
  -- * Etc.
  distances,
  initialAndFinalTasks,
  graphToMatrix,
  ShortestPath(..),
  extract,
  Language(..),
  letter,
  someWord
  ) where

import Control.Monad (guard)
import Data.Bifunctor (Bifunctor (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Monoid (Sum (..))
import Data.Reflection qualified as R
import Data.Semiring (Additive (..), RT, Real_ (..), Semiring (..),
                      StarSemiring (..), Tropical (..), inf, rt)
import Data.Set qualified as Set
import Data.TDigest (TDigest)
import Data.TDigest qualified as TDigest
import Ernie.Chart (DependencyGraph (..), GraphStructure (..), TaskID (..),
                    structure)
import FW.Matrix (SomeMatrix (..))
import FW.Matrix qualified as M

data TaskMeasure =
  TaskMeasure
    { tmDuration      :: !(TDigest 25)
    , tmCritPathCount :: !(Sum Int)
    , tmTotalCount    :: !(Sum Int)
    }

instance Semigroup TaskMeasure where
  l <> r =
    TaskMeasure
      { tmDuration = tmDuration l <> tmDuration r
      , tmCritPathCount = tmCritPathCount l <> tmCritPathCount r
      , tmTotalCount = tmTotalCount l <> tmTotalCount r
      }

instance Monoid TaskMeasure where
  mempty = TaskMeasure mempty mempty mempty

measure :: DependencyGraph TaskID Duration -> Map TaskID TaskMeasure
measure graph =
  let cpIDs = maybe Set.empty (Set.fromList . fst) (criticalPath graph)
      DependencyGraph{unDependencyGraph} = graph
      m taskID (_, Duration duration) =
        TaskMeasure
          { tmDuration = TDigest.singleton duration
          , tmCritPathCount = if taskID `Set.member` cpIDs then 1 else 0
          , tmTotalCount = 1
          }
  in Map.mapWithKey m unDependencyGraph

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

data ClosedTaskID =
  InitialTask
  | FinalTask
  | SomeTask TaskID
  deriving stock (Eq, Ord, Show)

initialAndFinalTasks :: Monoid n => DependencyGraph TaskID n -> DependencyGraph ClosedTaskID n
initialAndFinalTasks graph =
  let DependencyGraph{unDependencyGraph} = graph
      GraphStructure{gsInitialTasks, gsFinalTasks} = structure graph
      old = (first SomeTask . second (first (Set.map SomeTask))) <$> Map.toList unDependencyGraph
      initialLinks = fmap (\taskID -> (SomeTask taskID, (Set.singleton InitialTask, mempty))) (Set.toList gsInitialTasks)
      finalLinks = [(FinalTask, (Set.map SomeTask gsFinalTasks, mempty))]
  in DependencyGraph
      $ Map.fromListWith (<>)
      $ old
        ++ initialLinks
        ++ finalLinks

-- TODO: StarSemring instance for Map!

graphToMatrix :: Ord k => DependencyGraph k n -> (SomeMatrix (Maybe n), Map Int k)
graphToMatrix graph =
  let GraphStructure{gsAllTasks} = structure graph
      indices = Map.fromList $ zip [1..] (Set.toList gsAllTasks)
      DependencyGraph{unDependencyGraph} = graph
      matrixSize = Set.size gsAllTasks
      pathFromTo (source, target) = do
        (s', tgt') <- (,) <$> Map.lookup source indices <*> Map.lookup target indices
        (deps, e) <- Map.lookup tgt' unDependencyGraph
        guard (s' `Set.member` deps)
        return e
      m = R.reifyNat (fromIntegral matrixSize) $ \proxy -> SomeMatrix (M.mkMatrix' proxy pathFromTo)
  in (m, indices)

distances :: SomeMatrix (Maybe n) -> SomeMatrix (ShortestPath n (Language Int))
distances graph =
  let g (row, col) (Just n) =
        let cel = if row == col then letter row else letter row
        in ShortestPath (rt n) cel
      g _ Nothing = ShortestPath inf zero
  in M.mapMatrix (M.mapPos g) graph

newtype Duration = Duration Double
  deriving newtype (Num, Fractional, Show, Ord, Eq)
  deriving (Semigroup, Monoid) via (Sum Double)

{-| The critical path is the longest path from start to finish
-}
criticalPath :: DependencyGraph TaskID Duration -> Maybe ([TaskID], Duration)
criticalPath graph =
  let g' = fmap negate graph
  in fmap (fmap negate) (shortestPath g')

{-| Shortest path between beginning and end
-}
shortestPath :: (Ord n, Monoid n, Num n) => DependencyGraph TaskID n -> Maybe ([TaskID], n)
shortestPath graph =
  let (matrix, keys) = graphToMatrix (initialAndFinalTasks graph)
      invertedKeys = Map.fromList $ fmap (\(x, y) -> (y, x)) $ Map.toList keys
      dists = distances matrix
      source = fromMaybe (error "source") (Map.lookup InitialTask invertedKeys)
      target = fromMaybe (error "target") (Map.lookup FinalTask invertedKeys)
      result = M.withMatrix (flip M.at (source, target) . star) dists
      lkpIdx idx = fromMaybe (error "lkpIdx") (Map.lookup idx keys)
  in case result of
      ShortestPath (Real_ (Tropical n)) l ->
        let k (SomeTask i) = Just i
            k _            = Nothing
            w = maybe [] (mapMaybe k . fmap lkpIdx) (someWord l)
        in Just (w, n)
      _ -> Nothing

data ShortestPath a b = ShortestPath (RT a) b
  deriving (Show, Functor)

extract :: ShortestPath a b -> b
extract (ShortestPath _ b) = b

instance (Ord a, Num a, Additive b) => Additive (ShortestPath a b) where
  zero = ShortestPath zero zero
  ShortestPath a x .+. ShortestPath b y
    | c < b = ShortestPath a x
    | c < a = ShortestPath b y
    | otherwise = ShortestPath c (x .+. y)
    where c = a .+. b

instance (Ord a, Num a, Semiring b) => Semiring (ShortestPath a b) where
  one = ShortestPath one one
  ShortestPath a x .*. ShortestPath b y =
    ShortestPath (a .*. b) (x .*. y)

instance (Ord a, Num a, StarSemiring b) => StarSemiring (ShortestPath a b) where
  star (ShortestPath x b) | x == one  = ShortestPath one (star b)
                          | otherwise = ShortestPath one one

newtype Language a = Language [[a]]
  deriving Show

letter :: a -> Language a
letter x = Language [[x]]

instance Additive (Language a) where
  zero = Language []
  Language x .+. Language y =
    Language (x `interleave` y) where
      []     `interleave` ys  = ys
      (x':xs) `interleave` ys = x' : (ys `interleave` xs)

instance Semiring (Language a) where
  one = Language [[]]
  Language x .*. Language y =
    Language (dovetail (++) x y) where
      dovetail f l1 l2 = concat $ go l1 (scanl (flip (:)) [] l2) where
        go [] _            = []
        go l1' (x':y':ys') = (zipWith f l1' x'):(go l1' (y':ys'))
        go l1'@(_:as) [x'] = (zipWith f l1' x'):(go as [x'])
        go _ _             = error "go"

instance StarSemiring (Language a) where
  star (Language l) = one .+. plusList (filter (not . null) l) where
    plusList [] = zero
    plusList l' = star (Language l') .*. (Language l')

someWord :: Language a -> Maybe [a]
someWord (Language l) = listToMaybe l
