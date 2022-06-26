{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}
{-| CLI for generating PERT charts
-}
module Ernie.CLI(
  runMain
  ) where

import Data.Aeson qualified as JSON
import Data.ByteString.Lazy qualified as BSL
import Ernie.Export (dotFile)
import Ernie.JSONTask (JSONTask, makeChart)
import Ernie.Sample (measureSamples)
import Options.Applicative (Parser, auto, customExecParser, disambiguate, help,
                            helper, idm, info, long, option, prefs, short,
                            showHelpOnEmpty, showHelpOnError, strOption, value)
import System.Exit (exitFailure)

runMain :: IO ()
runMain = do
  CliOpts{inFile, outFile, numSamples} <- customExecParser
                (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
                (info (helper <*> optionsParser) idm)
  tasks <- JSON.decode @[JSONTask] <$> BSL.readFile inFile
  case tasks of
    Nothing -> do
      putStrLn $ "Failed to read task list from " <> inFile
      exitFailure
    Just ts -> do
      putStrLn $ "Generating chart for " <> show (length ts) <> " tasks"
      case makeChart ts of
        Left err -> do
          putStrLn $ "Failed to generate chart: " <> show err
          exitFailure
        Right c -> do
          putStrLn $ "Taking " <> show numSamples <> " samples"
          m <- measureSamples numSamples c
          putStrLn $ "Writing result to " <> outFile
          dotFile outFile m

data CliOpts =
  CliOpts
    { inFile     :: FilePath
    , outFile    :: FilePath
    , numSamples :: Int
    }

optionsParser :: Parser CliOpts
optionsParser =
  CliOpts
    <$> strOption (short 'f' <> long "file" <> help "Input file with tasks")
    <*> strOption (short 'o' <> long "out" <> help "Output .dot file" <> value "pert-chart.dot")
    <*> option auto (long "num-samples" <> help "How many samples to take" <> value 10_000)
