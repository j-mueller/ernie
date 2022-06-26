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
import Ernie.Chart (PERTChart, runChart, task)
import Ernie.Export (dotFile)
import Ernie.PERT (estimate)
import Ernie.Sample (measureSamples)

test :: IO ()
test = measureSamples 1000 example >>= dotFile "example.dot" . fst

example :: PERTChart
example = snd $ runChart $ do
  backend <- task "Build backend" (estimate 0.5 1.5 4) []
  frontend <- task "Build frontend" (estimate 2.0 4.5 7) [backend]
  docs <- task "Write docs" (estimate 3.0 4.0 9.0) [backend]
  void (task "Deploy" (estimate 3 3.5 8) [frontend, docs])
