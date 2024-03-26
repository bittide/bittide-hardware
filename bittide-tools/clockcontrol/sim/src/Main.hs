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

import Bittide.Plot
import Bittide.Topology
import Bittide.Simulate.Config

import Data.Aeson (decode)
import Data.ByteString.Lazy qualified as BS
import Data.Proxy (Proxy(..))

import Options.Applicative

import System.Exit (exitSuccess, die)

main :: IO ()
main = do
  simCfg@SimulationConfig{..} <- do
    cfg@SimulationConfig{..} <- execParser cliParser
    case jsonArgs of
      Nothing   -> return cfg
      Just file -> do
        cnt <- BS.readFile file
        case decode cnt of
          Nothing -> die $ "ERROR: Invalid JSON file - " <> file
          Just o  -> return o { jsonArgs }

  ccc <- clockControlConfig (Proxy @Bittide)
           (not disableReframing) rusty waitTime
           stabilityMargin stabilityFrameSize

  let
    saveToFile name g clockOffs startOffs isStable =
      saveSimConfig name g simCfg
        { stable   = isStable
        , clockOffsets = clockOffs
        , startupOffsets = startOffs
        }

    settings =
      SimulationSettings
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
        , ccConfig     = ccc
        , save         = \_ _ _ _ -> return ()
        }

  isStable <- case topology of
    Just t -> do
      let ?settings = settings { save = saveToFile $ topologyName t }
      case t of
        Diamond       -> plotDiamond
        Line n        -> plotLine n
        HyperCube n   -> plotHyperCube n
        Grid r c      -> plotGrid r c
        Torus2D r c   -> plotTorus2D r c
        Torus3D r c p -> plotTorus3D r c p
        Tree d c      -> plotTree d c
        Star n        -> plotStar n
        Cycle n       -> plotCyclic n
        Complete n    -> plotComplete n
        Hourglass n   -> plotHourglass n
        Random n      -> randomGraph n >>= plotGraph
        DotFile f     -> (fromDot <$> readFile f) >>= \case
          Right (g, name) ->
            let ?settings = ?settings { save = saveToFile name }
            in plotGraph g
          Left err -> die $ "ERROR: Invalid DOT file - " <> f <> "\n" <> err
    Nothing ->
      handleParseResult $ Failure
        $ parserFailure defaultPrefs cliParser (ShowHelpText Nothing) []

  if isStable
  then exitSuccess
  else die "Simulated topology did not stabilize in time."

 where
  cliParser = info (simConfigCLIParser <**> helper)
    $ fullDesc <> header "Bittide Hardware Topology Simulator"
