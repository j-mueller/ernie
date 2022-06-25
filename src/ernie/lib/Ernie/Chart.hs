{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-| PERT Charts with a simple monadic interface for
building them
-}
module Ernie.Chart(
  TaskID(..),
  task0,
  nextID,
  DependencyGraph(..),
  emptyGraph,
  structure,
  GraphStructure(..),
  PERTChart,
  -- * Building charts
  Chart,
  ChartT,
  MonadChart(..),
  task,
  runChart,
  runChartT
  ) where

import Control.Lens (_1, _Just, at, makePrisms, (%=), (.=))
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (..), gets, lift)
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
import Ernie.Task (Task (..))
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

newtype DependencyGraph k e = DependencyGraph { unDependencyGraph :: Map k (Set k, e) }
  deriving stock (Functor, Foldable, Traversable)

makePrisms ''DependencyGraph

emptyGraph :: DependencyGraph k e
emptyGraph = DependencyGraph Map.empty

{-| Information about the nodes in a dependency graph
-}
data GraphStructure k =
      GraphStructure
        { gsInitialTasks :: Set k -- ^ Tasks that don't depend on any other tasks
        , gsFinalTasks   :: Set k -- ^ Tasks that no other task depends on
        , gsAllTasks     :: Set k -- ^ The set of all task IDs that exist
        }

{-| The tasks that have no dependencies, and those that no other tasks
depend on.
-}
structure :: Ord k => DependencyGraph k e -> GraphStructure k
structure DependencyGraph{unDependencyGraph} =
  let targets = Map.keysSet unDependencyGraph
      sources = foldMap fst unDependencyGraph
      gsAllTasks = Set.union targets sources
      gsInitialTasks = Map.keysSet (Map.filter (Set.null . fst) unDependencyGraph)
      gsFinalTasks   = gsAllTasks `Set.difference` sources
  in GraphStructure{gsInitialTasks, gsFinalTasks, gsAllTasks}

{-| Class for constructing dependency graphs
-}
class Monad m => MonadChart m where
  type TaskDuration m :: Type

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
  -> (TaskDuration m) -- ^ Estimated duration
  -> [TaskID] -- ^ List of tasks that the new task depends on
  -> m TaskID -- ^ The ID of the new task
task taskName taskDuration deps = do
  t <- addTask Task{taskName, taskDuration}
  traverse_ (t `dependsOn`) deps
  pure t

newtype ChartT duration m a = ChartT { runChartT_ :: StateT (DependencyGraph TaskID (Task duration)) m a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadState s m => MonadState s (ChartT duration m) where
  get = ChartT (lift get)
  put s = ChartT (lift $ put s)

instance MonadError e m => MonadError e (ChartT duration m) where
  throwError = ChartT . lift . throwError
  catchError (ChartT m) f =
    let f' = fmap runChartT_ f
    in ChartT (catchError m f')

instance MonadChart m => MonadChart (StateT s m) where
  type TaskDuration (StateT s m) = TaskDuration m
  addTask = lift . addTask
  dependsOn a = lift . dependsOn a

nextID :: MonadState (DependencyGraph TaskID duration) m => m TaskID
nextID = maybe task0 (next . fst) <$> gets (Map.lookupMax . unDependencyGraph)

instance Monad m => MonadChart (ChartT duration m) where
  type TaskDuration (ChartT duration m) = duration
  addTask t = ChartT $ do
            i <- nextID
            _DependencyGraph . at i .= Just (mempty, t)
            pure i
  i1 `dependsOn` i2 = ChartT (_DependencyGraph . at i1 . _Just . _1 %= Set.insert i2)

type Chart duration = ChartT duration Identity

{-| Run a 'Chart' computation
-}
runChart :: Chart duration a -> (a, DependencyGraph TaskID (Task duration))
runChart = runIdentity . runChartT

{-| Run the 'ChartT' monad transformer
-}
runChartT :: ChartT duration m a -> m (a, DependencyGraph TaskID (Task duration))
runChartT = flip runStateT emptyGraph . runChartT_

type PERTChart = DependencyGraph TaskID (Task (PERTEstimate Days))
