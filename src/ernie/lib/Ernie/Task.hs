{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

-- | Tasks that make up a PERT chart
module Ernie.Task (
    Task (..),
    TType (..),
) where

import Data.Text (Text)

data TType = TTask | TEvent
    deriving stock (Eq, Show, Ord)

-- | Task with a name and duration
data Task d
    = Task
    { taskName :: Text
    , taskGroup :: Maybe Text
    , taskType :: TType
    , taskDuration :: d
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)
