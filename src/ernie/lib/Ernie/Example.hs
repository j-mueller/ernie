{-# LANGUAGE NamedFieldPuns #-}
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
import Ernie.Chart (PERTChart, TaskGraph (..), runChart, task)
import Ernie.Export (dotFile)
import Ernie.PERT (estimate)
import Ernie.Sample (Sample (..), sample1)
import Ernie.Task (Task (..))
import Ernie.Time (Days (..))

test :: IO ()
test = do
  TaskGraph g <- sample1 example
  flip traverse_ (Map.toAscList g) $ \(_, (_, Task{taskName, taskDuration = Sample (Days n)})) -> do
    putStrLn $ Text.unpack taskName <> ": " <> show n <> " days"
  dotFile "example.dot" example

example :: PERTChart
example = snd $ runChart $ do
  backend <- task "Build backend" (estimate 0.5 1.5 4) []
  frontend <- task "Build frontend" (estimate 2.0 4.5 7) [backend]
  void (task "Write docs" (estimate 3.0 4.0 9.0) [backend, frontend])
