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
import Ernie.Chart (PERTChart, runChart, task, task')
import Ernie.Export (dotFile)
import Ernie.PERT (estimate)
import Ernie.Sample (measureSamples)

test :: IO ()
test = measureSamples 1000 example >>= dotFile "example.dot" . fst

example :: PERTChart
example = snd $ runChart $ do
  let phase2 = Just "Phase 2"
  backend <- task "Build backend" (estimate 0.5 1.5 4) []
  frontend <- task' "Build frontend" (estimate 2.0 4.5 7) [backend] phase2
  userTest <- task' "User testing" (estimate 3 5 8) [frontend] phase2
  docs1 <- task' "Write docs (1)" (estimate 1.0 4.0 9.0) [backend] phase2
  docs2 <- task' "Write docs (2)" (estimate 2.0 4.0 9.0) [docs1, frontend] phase2
  void (task "Deploy" (estimate 3 3.5 8) [docs2, userTest])
