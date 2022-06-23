{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}
{-| Tasks that make up a PERT chart
-}
module Ernie.Task(
  Task(..),
  traverseTask
  ) where

import Data.Text (Text)
import Ernie.Time (Days)

data Task f =
  Task
    { taskName     :: Text
    , taskDuration :: f Days -- TODO: Support other units of time
    }

{-| A higher-kinded version of 'traverse' specialised to 'Days'
-}
traverseTask :: forall f g h. Applicative h => (f Days -> h (g Days)) -> Task f -> h (Task g)
traverseTask f Task{taskName, taskDuration} =
  Task taskName <$> f taskDuration
