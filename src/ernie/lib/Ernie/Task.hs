{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}
{-| Tasks that make up a PERT chart
-}
module Ernie.Task(
  Task(..)
  ) where

import Data.Text (Text)

{-| Task with a name and duration
-}
data Task d =
  Task
    { taskName     :: Text
    , taskDuration :: d
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)
