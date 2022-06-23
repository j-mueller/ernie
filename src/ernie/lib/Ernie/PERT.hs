{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData     #-}
{-| Core PERT types
-}
module Ernie.PERT(
  PERTEstimate(..),
  PERTQEstimate(..),
  estimate,
  pert,
  pert'
) where

import Control.Monad.Primitive (PrimMonad (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Profunctor (dimap)
import Ernie.Time (Days (..))
import GHC.Generics (Generic)
import System.Random.MWC.Probability (Prob)
import System.Random.MWC.Probability qualified as P

{-| Estimate of the duration of a task with three points: Minimum, maximum
and mode.
-}
data PERTEstimate n
  = PERTEstimate
      { pMin    :: n -- ^ Minimum duration
      , pMode   :: n -- ^ Typical (mode) duration
      , pMax    :: n -- ^ Maximum duration
      , pLambda :: n -- ^ Lambda parameter (default: 4)
      } deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
        deriving anyclass (ToJSON, FromJSON)

{-| Estimate of the duration of a task with three quantiles: 10%, 50%, 90%.
-}
data PERTQEstimate n
  = PERTQEstimate
      { pqTen    :: n -- ^ Ten percent quantile. Duration that will be *EXCEEDED* with a probability of 90%.
      , pqFifty  :: n -- ^ Typical (mode). Duration that will be exceeded with a probability of 50%. This is identical to 'pMode'.
      , pqNinety :: n -- ^ Ninety percent quantile. Duration that will be *EXCEEDED* with a probability of 10%.
      , pqLambda :: n -- ^ Lambda parameter 'pLambda'
      } deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
        deriving anyclass (ToJSON, FromJSON)

-- TODO: Transform PERTQEstimate to PERTEstimate
-- See https://towardsdatascience.com/python-scenario-analysis-modeling-expert-estimates-with-the-beta-pert-distribution-22a5e90cfa79

{-| A PERT estimate with the default value for lambda
-}
estimate :: Num n => n -> n -> n -> PERTEstimate n
estimate pMin pMode pMax =
  PERTEstimate{pMin, pMax, pMode, pLambda = 4}

{-| The distribution characterised by the estimate
-}
pert' :: PrimMonad m => PERTEstimate Double -> Prob m Double
pert' PERTEstimate{pMin, pMax, pMode, pLambda} = do
  -- TODO: We should really do some validation of the parameters here (all must be positive, pMin <= pMode <= pMax)
  -- see https://www.riskamp.com/beta-pert
  let range = pMax - pMin
      mu = (pMin + pMax + pLambda * pMode) / (pLambda + 2)
      v =
        if mu == pMode
          then (pLambda / 2) + 1
          else ((mu - pMin) * (2 * pMode - pMin - pMax)) / ((pMode - mu) * (pMax - pMin))
      w = (v * (pMax - mu)) / (mu - pMin)
  x <- P.beta v w
  -- TODO: Maybe this estimate should be the rate parameter of an exponential distribution
  -- to account for the possibility of misjudging the min/max values?
  pure (x * range + pMin)

pert :: PrimMonad m => PERTEstimate Days -> Prob m Days
pert = dimap (fmap getDays) (fmap Days) pert'
