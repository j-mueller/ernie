{-# LANGUAGE TupleSections #-}

-- | Sample from a PERT Chart
module Ernie.Sample (
    Sample (..),
    measureSamples,
    sampleChart,
    sampleChart',
    sample1,
    samplePERT,
) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Ernie.Chart (DependencyGraph (..), PERTChart, TaskID)
import Ernie.Measure (ClosedTaskID (..), TaskMeasure)
import Ernie.Measure qualified as M
import Ernie.PERT (PERTEstimate, pert)
import Ernie.Task (Task (..))
import Ernie.Time (Days (..))
import Streaming (Of, Stream)
import Streaming.Prelude qualified as S
import System.Random.MWC.Probability (Gen, createSystemRandom, sample)

newtype Sample n = Sample {getSample :: n}
    deriving stock (Eq, Ord, Show)

-- | Sample the distribution of the PERTEstimate
samplePERT :: (PrimMonad m) => Gen (PrimState m) -> Maybe (PERTEstimate Days) -> m (Sample Days)
samplePERT gen (Just t) = Sample <$> sample (pert t) gen
samplePERT _ Nothing = pure (Sample 0)

-- | An infinite stream of samples from the PERT chart distribution, using the provided generator
sampleChart' :: (PrimMonad m) => Gen (PrimState m) -> DependencyGraph k (Task (Maybe (PERTEstimate Days))) -> Stream (Of (DependencyGraph k (Task (Sample Days)))) m r
sampleChart' gen = S.repeatM . traverse (traverse (samplePERT gen))

-- | An infinite stream of samples from the PERT chart distribution
sampleChart :: PERTChart -> IO (Stream (Of (DependencyGraph TaskID (Task (Sample Days)))) IO r)
sampleChart chart = sampleChart' <$> createSystemRandom <*> pure chart

{- | Draw one sample from the distribution of task durations.
Consider using 'sampleChart' if you need more than one sample.
-}
sample1 :: PERTChart -> IO (DependencyGraph TaskID (Task (Sample Days)))
sample1 chart = sampleChart chart >>= fmap fromJust . S.head_

-- | Take a number of samples and summarise them using 'M.measure'
measureSamples :: Int -> PERTChart -> IO (DependencyGraph TaskID (Task (Maybe (PERTEstimate Days), TaskMeasure)), TaskMeasure)
measureSamples n chart = do
    samples <- sampleChart chart
    let k Task{taskDuration = Sample (Days n')} = M.Duration n'
    measures <- S.fold_ (Map.unionWith (<>)) mempty id $ S.map (M.measure . fmap k) $ S.take n samples

    let l taskID (deps, estimate) =
            let m = Map.findWithDefault mempty (SomeTask taskID) measures
                e' = fmap (,m) estimate
             in (deps, e')
        finalTaskMeasure = Map.findWithDefault mempty FinalTask measures
    return (DependencyGraph $ Map.mapWithKey l $ unDependencyGraph chart, finalTaskMeasure)
