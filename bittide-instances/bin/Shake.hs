-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Prelude

import Clash.Driver.Manifest
import Clash.Shake.Extra
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty((:|)))
import Development.Shake
import Development.Shake.Extra
import Development.Shake.FilePath ((</>))
import Language.Haskell.TH (nameBase)
import System.Console.ANSI (setSGR)

import Clash.Shake.Vivado

import qualified Bittide.Instances.Calendar as Calendar
import qualified Clash.Shake.Vivado.ParseTimingSummary as ParseTimingSummary
import qualified Clash.Util.Interpolate as I
import qualified Language.Haskell.TH as TH

buildDir :: FilePath
buildDir = "_build"

clashBuildDir :: FilePath
clashBuildDir = buildDir </> "clash"

vivadoBuildDir :: FilePath
vivadoBuildDir = buildDir </> "vivado"

-- | All synthesizable targets
targets :: [TH.Name]
targets =
  [ 'Calendar.switchCalendar1k
  , 'Calendar.switchCalendar1kReducedPins
  ]

shakeOpts :: ShakeOptions
shakeOpts = shakeOptions
  { shakeFiles = buildDir
  , shakeChange = ChangeDigest
  , shakeVersion = "5"
  }

-- | Run Vivado on given TCL script
vivadoFromTcl :: FilePath -> Action ()
vivadoFromTcl tclPath =
  command_
    [AddEnv "XILINX_LOCAL_USER_DATA" "no"] -- Prevents multiprocessing issues
    "vivado"
    ["-mode", "batch", "-source", tclPath]

-- | Defines a Shake build executable for calling Vivado. Like Make, in Shake
-- you define rules that explain how to build a certain file. For example:
--
--     manifestPath %> ...
--
-- means: to build @manifestPath@ I need to do dot-dot-dot. See the README for
-- an overview of which commands are user-passable (or simply scroll down).
--
-- For a fundamental introduction into Shake, read the (lightweight!) paper
-- introducing it:
--
--   https://ndmitchell.com/downloads/paper-shake_before_building-10_sep_2012.pdf.
--
-- Or, see https://shakebuild.com/.
--
main :: IO ()
main = do
  shakeArgs shakeOpts $ do
    -- For each target, generate a user callable command (PHONY). Run with
    -- '--help' to list them.
    for_ targets $ \target -> do
      let
        -- TODO: Dehardcode these paths. They're currently hardcoded in both the
        --       TCL and here, which smells.
        manifestPath = getManifestLocation clashBuildDir target
        synthesisDir = vivadoBuildDir </> show target
        falsePathXdc = synthesisDir </> "false_paths.xdc"
        checkpointsDir = synthesisDir </> "checkpoints"
        reportDir = synthesisDir </> "reports"

        runSynthTclPath   = synthesisDir </> "run_synth.tcl"
        runPlaceTclPath   = synthesisDir </> "run_place.tcl"
        runRouteTclPath   = synthesisDir </> "run_route.tcl"
        runNetlistTclPath = synthesisDir </> "run_netlist.tcl"

        postSynthCheckpointPath = checkpointsDir </> "post_synth.dcp"
        postPlaceCheckpointPath = checkpointsDir </> "post_place.dcp"
        postRouteCheckpointPath = checkpointsDir </> "post_route.dcp"

        postRouteTimingSummaryPath = reportDir </> "post_route_timing_summary.rpt"
        postRouteTimingPath = reportDir </> "post_route_timing.rpt"

        synthReportsPaths = [reportDir </> "post_synth_timing_summary.rpt"]
        placeReportPaths = [reportDir </> "post_place_timing_summary.rpt"]
        routeReportsPaths =
          [ reportDir </> "post_route_clock_util.rpt"
          , reportDir </> "post_route_drc.rpt"
          , reportDir </> "post_route_power.rpt"
          , reportDir </> "post_route_timing.rpt"
          , reportDir </> "post_route_timing_summary.rpt"
          , reportDir </> "post_route_util.rpt"
          ]

      withoutTargets $ do
        manifestPath %> \path -> do
          needDirectory "dist-newstyle"
          let (buildTool, buildToolArgs) = defaultClashCmd clashBuildDir target
          command_ [] buildTool buildToolArgs

          -- Clash messes up ANSI escape codes, leaving the rest of the terminal
          -- printed in bold text. Reset manually:
          liftIO (setSGR [])

          produces [path]

        falsePathXdc %> \path -> do
          LocatedManifest{lmManifest} <- decodeLocatedManifest manifestPath
          writeFileChanged path (mkFalsePathXdc lmManifest)

        -- Synthesis
        runSynthTclPath %> \path -> do
          need [falsePathXdc]
          locatedManifest <- decodeLocatedManifest manifestPath

          let
            LocatedManifest{lmManifest=Manifest{topComponent=lib}} = locatedManifest
            falsePathHdlSource = HdlSource XdcSource lib falsePathXdc

            !() =
              case transitiveDependencies (lmManifest locatedManifest) of
                [] -> ()
                _  -> error "Multiple libraries not yet implemented"

            tcl =
              mkSynthesisTcl
                synthesisDir            -- Output directory for Vivado
                False                   -- Out of context run
                "xcku035-ffva1156-2-e"  -- Part we're synthesizing for
                (locatedManifest :| []) -- We only support one library for now
                [falsePathHdlSource]    -- Extra files

          writeFileChanged path tcl

        (postSynthCheckpointPath : synthReportsPaths) |%> \_ -> do
          need [runSynthTclPath, manifestPath]
          vivadoFromTcl runSynthTclPath

        -- Placement
        runPlaceTclPath %> \path -> do
          writeFileChanged path (mkPlaceTcl synthesisDir)

        (postPlaceCheckpointPath : placeReportPaths) |%> \_ -> do
          need [runPlaceTclPath, postSynthCheckpointPath]
          vivadoFromTcl runPlaceTclPath

        -- Routing
        runRouteTclPath %> \path -> do
          writeFileChanged path (mkRouteTcl synthesisDir)

        (postRouteCheckpointPath : routeReportsPaths) |%> \_ -> do
          need [runRouteTclPath, postPlaceCheckpointPath]
          vivadoFromTcl runRouteTclPath

          -- Design should meet timing post routing. Note that this is not a
          -- requirement after synthesis as many of the optimizations only follow
          -- after.
          report <- liftIO (ParseTimingSummary.parseFile postRouteTimingSummaryPath)
          case ParseTimingSummary.meetsTiming report of
            Nothing -> pure ()
            Just wns -> error [I.i|
              Design did not meet timing. Negative WNS detected: #{wns}. Check out
              the timing summary at:

                #{postRouteTimingSummaryPath}

              Alternatively, check out the full report:

                #{postRouteTimingPath}

              You can investigate interactively by opening the latest checkpoint with Vivado:

                vivado #{postRouteCheckpointPath}

            |]

        -- Netlist generation
        runNetlistTclPath %> \path -> do
          writeFileChanged path (mkNetlistTcl synthesisDir)

      -- User friendly target names
      phony (nameBase target <> ":hdl") $ do
        need [manifestPath]

      phony (nameBase target <> ":synth") $ do
        need [postSynthCheckpointPath]

      phony (nameBase target <> ":place") $ do
        need [postPlaceCheckpointPath]

      phony (nameBase target <> ":route") $ do
        need [postRouteCheckpointPath]

      phony (nameBase target <> ":netlist") $ do
        need [postRouteCheckpointPath, runNetlistTclPath]
        vivadoFromTcl runNetlistTclPath
        produces [synthesisDir </> "netlist.v", synthesisDir </> "netlist.xdc"]
