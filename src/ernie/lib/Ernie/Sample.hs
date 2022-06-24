{-| Sample from a PERT Chart
-}
module Ernie.Sample(
  Sample(..),
  sampleChart,
  sampleChart',
  sample1,
  samplePERT
  ) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Maybe (fromJust)
import Ernie.Chart (DependencyGraph, PERTChart, TaskID)
import Ernie.PERT (PERTEstimate, pert)
import Ernie.Task (Task)
import Ernie.Time (Days)
import Streaming (Of, Stream)
import Streaming.Prelude qualified as S
import System.Random.MWC.Probability (Gen, createSystemRandom, sample)

newtype Sample n = Sample{ getSample :: n }
  deriving stock (Eq, Ord, Show)

{-| Sample the distribution of the PERTEstimate
-}
samplePERT :: PrimMonad m => Gen (PrimState m) -> PERTEstimate Days -> m (Sample Days)
samplePERT gen t = Sample <$> sample (pert t) gen

{-| An infinite stream of samples from the PERT chart distribution, using the provided generator
-}
sampleChart' :: PrimMonad m => Gen (PrimState m) -> DependencyGraph k (Task (PERTEstimate Days)) -> Stream (Of (DependencyGraph k (Task (Sample Days)))) m r
sampleChart' gen = S.repeatM . traverse (traverse (samplePERT gen))

{-| An infinite stream of samples from the PERT chart distribution
-}
sampleChart :: PERTChart -> IO (Stream (Of (DependencyGraph TaskID (Task (Sample Days)))) IO r)
sampleChart chart = sampleChart' <$> createSystemRandom <*> pure chart

{-| Draw one sample from the distribution of task durations.
Consider using 'sampleChart' if you need more than one sample.
-}
sample1 :: PERTChart -> IO (DependencyGraph TaskID (Task (Sample Days)))
sample1 chart = sampleChart chart >>= fmap fromJust . S.head_
