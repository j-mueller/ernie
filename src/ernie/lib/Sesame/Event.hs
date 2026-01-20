{-| Constraints and relations on tasks
-}
module Sesame.Event(
  Task(..),
  task,
  Event(..),
  event
) where

import Data.Time.Clock (UTCTime)
import Ernie.Time (Days)
import Sesame.Var (Var, Fresh(..))

-- | Task with beginning and end
data Task md =
  Task
    { tBegin    :: Var UTCTime
    , tDuration :: Var Days
    , tMetadata :: md
    }
    deriving stock (Eq, Show, Functor, Foldable, Traversable)

task :: Fresh m => md -> m (Task md)
task meta =
  Task <$> var <*> var <*> pure meta

-- | Events do not have a duration
data Event md =
  Event
    { eTime     :: Var UTCTime
    , eMetadata :: md
    }
    deriving stock (Eq, Show)

event :: Fresh m => md -> m (Event md)
event meta = Event <$> var <*> pure meta