-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
module Main (main) where

import Domain

import Bittide.Simulate
import Bittide.Simulate.Config
import Bittide.Topology

import Data.Aeson (decode)
import Data.ByteString.Lazy qualified as BS
import Data.Proxy (Proxy(..))

import Options.Applicative

import System.Exit (exitSuccess, die)

main :: IO ()
main = do
  simCfg@SimConf{..} <- do
    cfg@SimConf{..} <- execParser cliParser
    case jsonArgs of
      Nothing   -> return cfg
      Just file -> do
        cnt <- BS.readFile file
        case decode cnt of
          Nothing -> die $ "ERROR: Invalid JSON file - " <> file
          Just o  -> return o { jsonArgs }

  sccc <- someCCC (Proxy @Bittide)
    (not disableReframing) rusty waitTime
      (toInteger stabilityMargin) (toInteger stabilityFrameSize)

  isStable <- case mTopologyType of
    Nothing ->
      handleParseResult $ Failure
        $ parserFailure defaultPrefs cliParser (ShowHelpText Nothing) []
    Just tt -> fromTopologyType tt >>= \case
      Left err -> die $ "Error : " <> err
      Right t -> simPlot t SimPlotSettings
        { samples      = simulationSamples
        , periodsize   = simulationSteps `quot` simulationSamples
        , mode         = outMode
        , dir          = outDir
        , stopStable   =
            if stopWhenStable
            then Just 0
            else (`quot` simulationSamples) <$> stopAfterStable
        , fixClockOffs = clockOffsets
        , fixStartOffs = startupOffsets
        , maxStartOff  = maxStartupOffset
        , save         = \clockOffs startOffs isStable ->
            saveSimConfig t simCfg
              { stable         = isStable
              , clockOffsets   = clockOffs
              , startupOffsets = startOffs
              }
        , ..
        }

  if isStable
  then exitSuccess
  else die "Simulated topology did not stabilize in time."

 where
  cliParser = info (simConfigCLIParser <**> helper)
    $ fullDesc <> header "Bittide Hardware Topology Simulator"
