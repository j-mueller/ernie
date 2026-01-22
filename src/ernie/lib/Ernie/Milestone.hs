module Ernie.Milestone (
    Milestone (..),
) where

import Data.Text (Text)

-- | Milestone. An event, no duration
data Milestone
    = Milestone
    { msName :: Text
    , msGroup :: Maybe Text
    }
