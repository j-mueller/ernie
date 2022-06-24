{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}
{-| Example / testing module
-}
module Ernie.Example(
  test,
  example
  ) where

import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Ernie.Chart (DependencyGraph (..), GraphStructure (..), PERTChart,
                    runChart, structure, task)
import Ernie.Export (dotFile)
import Ernie.Measure (Duration (..), criticalPath)
import Ernie.PERT (estimate)
import Ernie.Sample (Sample (..), sample1)
import Ernie.Task (Task (..))
import Ernie.Time (Days (..))

test :: IO ()
test = do
  graph@(DependencyGraph g) <- sample1 example
  flip traverse_ (Map.toAscList g) $ \(_, (_, Task{taskName, taskDuration = Sample (Days n)})) -> do
    putStrLn $ Text.unpack taskName <> ": " <> show n <> " days"
  let GraphStructure{gsInitialTasks, gsFinalTasks} = structure graph
  putStrLn "Initial tasks:"
  traverse_ (putStrLn . (<>) "  " . show) gsInitialTasks
  putStrLn "Final tasks:"
  traverse_ (putStrLn . (<>) "  " . show) gsFinalTasks
  dotFile "example.dot" graph

  putStrLn "Critical path:"
  let g' = fmap (Duration . getDays . getSample . taskDuration) graph
  putStrLn ("  " <> show (criticalPath g'))

example :: PERTChart
example = snd $ runChart $ do
  backend <- task "Build backend" (estimate 0.5 1.5 4) []
  frontend <- task "Build frontend" (estimate 2.0 4.5 7) [backend]
  docs <- task "Write docs" (estimate 3.0 4.0 9.0) [backend]
  void (task "Deploy" (estimate 3 3.5 8) [frontend, docs])
