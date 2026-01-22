{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

-- | Example / testing module
module Ernie.Example (
    test,
    example,
) where

import Control.Monad (void)
import Ernie.Chart (PERTChart, runChart, task, task')
import Ernie.Export (dotFile)
import Ernie.PERT (estimate)
import Ernie.Sample (measureSamples)
import Ernie.Task (TType (..))

test :: IO ()
test = measureSamples 1000 example >>= dotFile "example.dot" . fst

example :: PERTChart
example = snd $ runChart $ do
    let phase2 = Just "Phase 2"
    backend <- task "Build backend" (Just $ estimate 0.5 1.5 4) []
    frontend <- task' "Build frontend" (Just $ estimate 2.0 4.5 7) [backend] phase2 TEvent
    userTest <- task' "User testing" (Just $ estimate 3 5 8) [frontend] phase2 TEvent
    docs1 <- task' "Write docs (1)" (Just $ estimate 1.0 4.0 9.0) [backend] phase2 TEvent
    docs2 <- task' "Write docs (2)" (Just $ estimate 2.0 4.0 9.0) [docs1, frontend] phase2 TEvent
    void (task "Deploy" (Just $ estimate 3 3.5 8) [docs2, userTest])
