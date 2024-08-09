-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Domain

import Bittide.Report.ClockControl
import Bittide.Simulate
import Bittide.Simulate.Config
import Bittide.Topology

import Control.Monad (when)
import Data.Aeson (decode)
import Data.ByteString.Lazy qualified as BS
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))

import Options.Applicative

import System.Exit (die, exitSuccess)

main :: IO ()
main = do
  simCfg@SimConf{..} <- do
    cfg@SimConf{..} <- execParser cliParser
    case jsonArgs of
      Nothing -> return cfg
      Just file -> do
        cnt <- BS.readFile file
        case decode cnt of
          Nothing -> die $ "ERROR: Invalid JSON file - " <> file
          Just o -> return o{jsonArgs}

  when createReport $
    checkDependencies
      >>= maybe (return ()) die

  sccc <-
    someCCC
      (Proxy @Bittide)
      reframe
      rusty
      waitTime
      (toInteger stabilityMargin)
      (toInteger stabilityFrameSize)

  isStable <- case mTopologyType of
    Nothing ->
      handleParseResult $
        Failure $
          parserFailure defaultPrefs cliParser (ShowHelpText Nothing) []
    Just tt ->
      fromTopologyType tt >>= \case
        Left err -> die $ "Error : " <> err
        Right t ->
          simPlot
            t
            SimPlotSettings
              { plotSamples = samples
              , periodsize = duration `quot` samples
              , mode = outMode
              , dir = outDir
              , stopStable =
                  if stopWhenStable
                    then Just 0
                    else (`quot` samples) <$> stopAfterStable
              , fixClockOffs = clockOffsets
              , fixStartDelays = startupDelays
              , maxStartDelay = maxStartupDelay
              , save = \clockOffs startDelays isStable -> do
                  let cfg =
                        simCfg
                          { stable = isStable
                          , clockOffsets = clockOffs
                          , startupDelays = startDelays
                          }
                  saveSimConfig t cfg
                  when (isJust isStable && createReport) $
                    checkIntermediateResults outDir
                      >>= maybe (generateReport "Simulation Report" outDir [] cfg) die
              , ..
              }

  if isStable
    then exitSuccess
    else die "Simulated topology did not stabilize in time."
 where
  cliParser =
    info (simConfigCLIParser <**> helper) $
      fullDesc <> header "Bittide Hardware Topology Simulator"
