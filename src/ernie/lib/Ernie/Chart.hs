{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-| PERT Charts with a simple monadic interface for
building them
-}
module Ernie.Chart(
  TaskID,
  task0,
  nextID,
  TaskGraph(..),
  emptyGraph,
  traverseGraph,
  PERTChart,
  -- * Building charts
  Chart,
  ChartT,
  MonadChart(..),
  task,
  runChart,
  runChartT
  ) where

import Control.Lens (_2, _Just, at, makePrisms, (%=), (.=))
import Control.Monad.State (MonadState, gets)
import Control.Monad.Trans.State.Strict (StateT (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Ernie.PERT (PERTEstimate)
import Ernie.Task (Task (..), traverseTask)
import Ernie.Time (Days)
import GHC.Generics (Generic)

newtype TaskID = TaskID Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

{-| First task ID
-}
task0 :: TaskID
task0 = TaskID 0

{-| Next task ID
-}
next :: TaskID -> TaskID
next (TaskID i) = TaskID (succ i)

newtype TaskGraph e = TaskGraph { unTaskGraph :: Map TaskID (Task e, Set TaskID) }

traverseGraph :: forall f g h. Applicative h => (f Days -> h (g Days)) -> TaskGraph f -> h (TaskGraph g)
traverseGraph f (TaskGraph g) =
  let k (t, s) = (,) <$> traverseTask f t <*> pure s
  in TaskGraph <$> traverse k g

makePrisms ''TaskGraph

emptyGraph :: TaskGraph e
emptyGraph = TaskGraph Map.empty

{-| Class for constructing dependency graphs
-}
class Monad m => MonadChart m where
  type TaskDuration m :: Type -> Type

  {-| Add a new task
  -}
  addTask :: Task (TaskDuration m) -> m TaskID

  {-| Record a dependency between tasks. @depends a b@ means that @a@ depends on @b@.
  -}
  dependsOn :: TaskID -> TaskID -> m ()

{-| Add a task
-}
task ::
  MonadChart m
  => Text -- ^ name of the task
  -> (TaskDuration m) Days -- ^ Estimated duration
  -> [TaskID] -- ^ List of tasks that the new task depends on
  -> m TaskID -- ^ The ID of the new task
task taskName taskDuration deps = do
  t <- addTask Task{taskName, taskDuration}
  traverse_ (t `dependsOn`) deps
  pure t

newtype ChartT (duration :: Type -> Type) m a = ChartT { runChartT_ :: StateT (TaskGraph duration) m a }
  deriving newtype (Functor, Applicative, Monad)

nextID :: MonadState (TaskGraph duration) m => m TaskID
nextID = maybe task0 (next . fst) <$> gets (Map.lookupMax . unTaskGraph)

instance Monad m => MonadChart (ChartT duration m) where
  type TaskDuration (ChartT duration m) = duration
  addTask t = ChartT $ do
            i <- nextID
            _TaskGraph . at i .= Just (t, mempty)
            pure i
  i1 `dependsOn` i2 = ChartT (_TaskGraph . at i1 . _Just . _2 %= Set.insert i2)

type Chart duration = ChartT duration Identity

{-| Run a 'Chart' computation
-}
runChart :: Chart duration a -> (a, TaskGraph duration)
runChart = runIdentity . runChartT

{-| Run the 'ChartT' monad transformer
-}
runChartT :: ChartT duration m a -> m (a, TaskGraph duration)
runChartT = flip runStateT emptyGraph . runChartT_

type PERTChart = TaskGraph PERTEstimate
