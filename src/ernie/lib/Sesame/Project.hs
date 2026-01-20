-- | Description of tasks and dependencies
module Sesame.Project(
  ProjectState(..)
) where

import Data.Text (Text)
import Sesame.Event (Event, Task)
import Sesame.Relation (Constraint)

data ProjectState =
  ProjectState
    { psTasks       :: [Task Text]
    , psEvents      :: [Event Text]
    , psConstraints :: [Constraint]
    }
    deriving stock (Show)

-- 