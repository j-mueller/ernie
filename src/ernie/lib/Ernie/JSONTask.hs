{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
{-| JSON format for tasks
-}
module Ernie.JSONTask(
  JSONTask
  , JSONTaskError(..)
  , makeChart
  ) where

import Control.Monad.Except (MonadError, runExcept, throwError)
import Control.Monad.State.Strict (MonadState, execStateT, gets, modify)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.:), (.=))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Foldable (toList, traverse_)
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Ernie.Chart (MonadChart (..), PERTChart, TaskID, runChartT)
import Ernie.PERT (PERTEstimate (..))
import Ernie.Task (Task (..))
import Ernie.Time (Days (..))
import GHC.Generics (Generic)

data JSONEstimate =
  JSONEstimate
    { eMin  :: Double
    , eMode :: Double
    , eMax  :: Double
    } deriving (Eq, Ord, Show)

instance ToJSON JSONEstimate where
  toJSON JSONEstimate{eMin, eMode, eMax} =
    object ["min" .= eMin, "mode" .= eMode, "max" .= eMax]

instance FromJSON JSONEstimate where
  parseJSON (Object v) =
    JSONEstimate
      <$> v .: "min"
      <*> v .: "mode"
      <*> v .: "max"
  parseJSON (Array vl) = do
    xl <- traverse parseJSON (toList vl)
    case xl of
      [eMin, eMode, eMax] -> pure JSONEstimate{eMin, eMode, eMax}
      _                   -> typeMismatch "Array with three elements" (Array vl)
  parseJSON invalid =
        prependFailure "parsing JSONEstimate failed, "
            (typeMismatch "Object" invalid)

data JSONTask =
  JSONTask
    { name     :: Text
    , key      :: Maybe Text
    , group    :: Maybe Text
    , estimate :: JSONEstimate
    , depends  :: Maybe [Text] -- this is a 'Maybe' so that the generically derived JSON parser makes this an optional field
    } deriving stock (Eq, Ord, Show, Generic)
      deriving anyclass (ToJSON, FromJSON)

type JSONTaskState = Map Text TaskID

data JSONTaskError =
  KeyNotFound Text
  deriving stock Show

makeChart :: [JSONTask] -> Either JSONTaskError PERTChart
makeChart tasks = runExcept $ fmap snd $ runChartT $ flip execStateT Map.empty $ do
  -- we make two passes over the tasks so that the user doesn't have to order
  -- them topologically (which could be quite annoying)
  traverse_ addJSONTask tasks
  traverse_ addJSONTaskDependencies tasks

{-| Add a task (without recording its dependencies)
-}
addJSONTask :: (TaskDuration m ~ (PERTEstimate Days), MonadChart m, MonadState JSONTaskState m) => JSONTask -> m ()
addJSONTask JSONTask{name, key, estimate=JSONEstimate{eMin, eMode, eMax}, group} = do
  let k = fromMaybe name key
      e = PERTEstimate{pMin = eMin, pMode = eMode, pMax = eMax, pLambda = 4}
  tid <- addTask Task{taskName = name, taskDuration = Days <$> e, taskGroup = group}
  modify (Map.insert k tid)

findKey :: (MonadError JSONTaskError m, MonadState JSONTaskState m) => Text -> m TaskID
findKey t = gets (Map.lookup t) >>= maybe (throwError $ KeyNotFound t) pure

addJSONTaskDependencies :: (MonadError JSONTaskError m, MonadChart m, MonadState JSONTaskState m) => JSONTask -> m ()
addJSONTaskDependencies JSONTask{name, key, depends} = do
  k <- findKey (fromMaybe name key)
  let register t = findKey t >>= (k `dependsOn`)
  maybe (pure ()) (traverse_ register) depends
