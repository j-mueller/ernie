{-| Sample from a PERT Chart
-}
module Ernie.Sample(
  Sample(..),
  sampleChart,
  sampleChart',
  sample1
  ) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Maybe (fromJust)
import Ernie.Chart (PERTChart, TaskGraph, traverseGraph)
import Ernie.PERT (pert)
import Streaming (Of, Stream)
import Streaming.Prelude qualified as S
import System.Random.MWC.Probability (Gen, createSystemRandom, sample)

newtype Sample n = Sample{ getSample :: n }
  deriving stock (Eq, Ord, Show)

{-| An infinite stream of samples from the PERT chart distribution, using the provided generator
-}
sampleChart' :: PrimMonad m => Gen (PrimState m) -> PERTChart -> Stream (Of (TaskGraph Sample)) m r
sampleChart' gen chart =
  let f _ = do
        c' <- traverseGraph (\t -> Sample <$> sample (pert t) gen) chart
        pure (Right (c', ()))
  in S.unfoldr f ()

{-| An infinite stream of samples from the PERT chart distribution
-}
sampleChart :: PERTChart -> IO (Stream (Of (TaskGraph Sample)) IO r)
sampleChart chart = sampleChart' <$> createSystemRandom <*> pure chart

{-| Draw one sample from the distribution of task durations.
Consider using 'sampleChart' if you need more than one sample.
-}
sample1 :: PERTChart -> IO (TaskGraph Sample)
sample1 chart = sampleChart chart >>= fmap fromJust . S.head_
