{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}
{-| CLI for generating PERT charts
-}
module Ernie.CLI(
  runMain
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import Control.Monad.Except (liftIO, runExceptT, throwError)
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy qualified as BSL
import Data.TDigest qualified as TDigest
import Ernie.Export (dotFile)
import Ernie.JSONTask (JSONTask, JSONTaskError, makeChart)
import Ernie.Measure (TaskMeasure (..))
import Ernie.Sample (measureSamples)
import Options.Applicative (Parser, auto, customExecParser, disambiguate, help,
                            helper, idm, info, long, option, prefs, short,
                            showHelpOnEmpty, showHelpOnError, strOption, switch,
                            value)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, takeFileName)
import System.FSNotify qualified as FSNotify

runMain :: IO ()
runMain = do
  CliOpts{inFile, outFile, numSamples, watch} <- customExecParser
                (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
                (info (helper <*> optionsParser) idm)
  if watch
    then do
      putStrLn $ "Watching " <> inFile <> " for changes"
      FSNotify.withManager $ \mgr -> do
        let fileModified (FSNotify.Modified fp' _ _) = takeFileName fp' == takeFileName inFile
            fileModified _                           = False
            gen = genChart inFile outFile numSamples >>= either printError pure
        gen -- generate once, then wait for changes
        void $ FSNotify.watchDir
                mgr
                (takeDirectory inFile)
                fileModified
                (const $ threadDelay 500 >> gen)
        forever $ threadDelay 1000000
    else genChart inFile outFile numSamples >>= either exitOnFailure pure

data GenChartError =
  FailedToReadTaskList FilePath
  | FailedToGenerateChart JSONTaskError

printError :: GenChartError -> IO ()
printError = \case
  FailedToReadTaskList fp -> putStrLn $ "Failed to read task list from " <> fp
  FailedToGenerateChart err -> putStrLn $ "Failed to generate chart: " <> show err

exitOnFailure :: GenChartError -> IO ()
exitOnFailure e = printError e >> exitFailure

genChart :: FilePath -> FilePath -> Int -> IO (Either GenChartError ())
genChart inf outf samples = runExceptT $ do
  tasks <- liftIO (JSON.decode @[JSONTask] <$> BSL.readFile inf)
  case tasks of
    Nothing -> throwError $ FailedToReadTaskList inf
    Just ts -> do
      liftIO (putStrLn $ "Generating chart for " <> inf <> " with " <> show (length ts) <> " tasks")
      case makeChart ts of
        Left err -> throwError (FailedToGenerateChart err)
        Right c -> liftIO $ do
          putStrLn $ "  Taking " <> show samples <> " samples"
          (m, TaskMeasure{tmTotalDuration}) <- measureSamples samples c
          let showDuration d = maybe "" (take 4 . show) (TDigest.quantile d tmTotalDuration)
          putStrLn $ "  Total project duration:"
          putStrLn $ "    *  5%: " <> showDuration 0.05
          putStrLn $ "    * 50%: " <> showDuration 0.5
          putStrLn $ "    * 95%: " <> showDuration 0.95
          putStrLn $ "  Writing result to " <> outf
          dotFile outf m

data CliOpts =
  CliOpts
    { inFile     :: FilePath
    , outFile    :: FilePath
    , numSamples :: Int
    , watch      :: Bool
    }

optionsParser :: Parser CliOpts
optionsParser =
  CliOpts
    <$> strOption (short 'f' <> long "file" <> help "Input file with tasks")
    <*> strOption (short 'o' <> long "out" <> help "Output .dot file" <> value "pert-chart.dot")
    <*> option auto (long "num-samples" <> help "How many samples to take" <> value 10_000)
    <*> switch (short 'w' <> long "watch" <> help "Watch the input file for changes and regenerate the output every time the file is changed")
