-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Prelude

import Control.Monad.Extra (ifM, unlessM)
import Data.Foldable (for_)
import Development.Shake
import GHC.Stack (HasCallStack)
import Language.Haskell.TH (nameBase)
import System.Console.ANSI (setSGR)
import System.Directory (getCurrentDirectory)
import System.FilePath (isDrive, (</>), takeDirectory)

import Paths_bittide_instances

import Clash.Shake.Vivado
import Clash.Shake.Extra
import Development.Shake.Extra

import qualified Bittide.Instances.Calendar as Calendar
import qualified Bittide.Instances.ClockControl as ClockControl
import qualified Bittide.Instances.Counter as Counter
import qualified Bittide.Instances.ElasticBuffer as ElasticBuffer
import qualified Bittide.Instances.MVPs as MVPs
import qualified Bittide.Instances.ProcessingElement as ProcessingElement
import qualified Bittide.Instances.ScatterGather as ScatterGather
import qualified Bittide.Instances.Si539xSpi as Si539xSpi
import qualified Bittide.Instances.StabilityChecker as StabilityChecker
import qualified Bittide.Instances.Synchronizer as Synchronizer
import qualified Clash.Util.Interpolate as I
import qualified Language.Haskell.TH as TH
import qualified System.Directory as Directory

-- | Given Cabal project root, determine build directory
buildDir :: FilePath -> FilePath
buildDir projectRoot = projectRoot </> "_build"

-- | Given Cabal project root, determine Clash HDL output directory
clashBuildDir :: FilePath -> FilePath
clashBuildDir projectRoot = buildDir projectRoot </> "clash"

-- | Given Cabal project root, determine directory for Vivado input + output files
vivadoBuildDir :: FilePath -> FilePath
vivadoBuildDir projectRoot = buildDir projectRoot </> "vivado"

getConstraintFileName :: TH.Name -> IO FilePath
getConstraintFileName target =
  getDataFileName ("data" </> "constraints" </> nameBase target <> ".xdc")


-- | Searches for a file called @cabal.project@ It will look for it in the
-- current working directory. If it can't find it there, it will traverse up
-- until it finds the file.
--
-- The returned path points to the directory containing @cabal.project@. Errors
-- if it could not find @cabal.project@ anywhere.
--
findProjectRoot :: HasCallStack => IO FilePath
findProjectRoot = goUp =<< getCurrentDirectory
 where
  goUp :: FilePath -> IO FilePath
  goUp path
    | isDrive path = error "Could not find 'cabal.project'"
    | otherwise =
        ifM
          (Directory.doesFileExist (path </> projectFilename))
          (return path)
          (goUp (takeDirectory path))

  projectFilename = "cabal.project"

-- | All synthesizable targets
targets :: [TH.Name]
targets =
  [ 'Calendar.switchCalendar1k
  , 'Calendar.switchCalendar1kReducedPins
  , 'ClockControl.callisto3
  , 'Counter.counterReducedPins
  , 'ElasticBuffer.elasticBuffer5
  , 'MVPs.clockControlDemo0
  , 'MVPs.clockControlDemo1
  , 'ProcessingElement.vexRiscAxiLoopback
  , 'ProcessingElement.vexRiscUartEcho
  , 'ScatterGather.gatherUnit1K
  , 'ScatterGather.gatherUnit1KReducedPins
  , 'ScatterGather.scatterUnit1K
  , 'ScatterGather.scatterUnit1KReducedPins
  , 'Si539xSpi.callistoSpi
  , 'Si539xSpi.si5391Spi
  , 'StabilityChecker.stabilityChecker_3_1M
  , 'Synchronizer.safeDffSynchronizer
  ]

shakeOpts :: FilePath -> ShakeOptions
shakeOpts projectRoot = shakeOptions
  { shakeFiles = buildDir projectRoot
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
  projectRoot <- findProjectRoot

  shakeArgs (shakeOpts projectRoot) $ do
    -- 'all' builds all targets defined below
    phony "all" $ do
      for_ targets $ \target -> do
        need [nameBase target <> ":synth"]

    -- For each target, generate a user callable command (PHONY). Run with
    -- '--help' to list them.
    for_ targets $ \target -> do
      let
        -- TODO: Dehardcode these paths. They're currently hardcoded in both the
        --       TCL and here, which smells.
        manifestPath = getManifestLocation (clashBuildDir projectRoot) target
        synthesisDir = vivadoBuildDir projectRoot </> show target
        falsePathXdc = synthesisDir </> "false_paths.xdc"
        checkpointsDir = synthesisDir </> "checkpoints"
        netlistDir = synthesisDir </> "netlist"
        reportDir = synthesisDir </> "reports"

        runSynthTclPath        = synthesisDir </> "run_synth.tcl"
        runPlaceTclPath        = synthesisDir </> "run_place.tcl"
        runRouteTclPath        = synthesisDir </> "run_route.tcl"
        runNetlistTclPath      = synthesisDir </> "run_netlist.tcl"
        runBitstreamTclPath    = synthesisDir </> "run_bitstream.tcl"
        runBoardProgramTclPath = synthesisDir </> "run_board_program.tcl"

        postSynthCheckpointPath = checkpointsDir </> "post_synth.dcp"
        postPlaceCheckpointPath = checkpointsDir </> "post_place.dcp"
        postRouteCheckpointPath = checkpointsDir </> "post_route.dcp"

        netlistPaths =
          [ netlistDir </> "netlist.v"
          , netlistDir </> "netlist.xdc"
          ]
        bitstreamPath = synthesisDir </> "bitstream.bit"

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
          needDirectory (projectRoot </> "dist-newstyle")
          let (buildTool, buildToolArgs) = defaultClashCmd (clashBuildDir projectRoot) target
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
          synthesisPart <- getEnvWithDefault "xcku035-ffva1156-2-e" "SYNTHESIS_PART"
          locatedManifest <- decodeLocatedManifest manifestPath

          constraintFileName <- liftIO (getConstraintFileName target)
          constraintExists <- liftIO $ Directory.doesFileExist constraintFileName
          let constraintList = ([constraintFileName | constraintExists])

          -- let
          --   LocatedManifest{lmManifest=Manifest{topComponent=lib}} = locatedManifest
          --   falsePathHdlSource = HdlSource XdcSource lib falsePathXdc

          tcl <- liftIO $
            mkSynthesisTcl
              synthesisDir            -- Output directory for Vivado
              False                   -- Out of context run
              synthesisPart           -- Part we're synthesizing for
              constraintList          -- List of filenames with constraints
              locatedManifest
              -- [falsePathHdlSource]    -- Extra files

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
          liftIO $ unlessM
            (meetsTiming postRouteTimingSummaryPath)
            (error [I.i|
              Design did not meet timing. Check out the timing summary at:

                #{postRouteTimingSummaryPath}

              Alternatively, check out the full report:

                #{postRouteTimingPath}

              You can investigate interactively by opening the latest checkpoint with Vivado:

                vivado #{postRouteCheckpointPath}

            |])

        -- Netlist generation
        runNetlistTclPath %> \path -> do
          writeFileChanged path (mkNetlistTcl synthesisDir)

        netlistPaths |%> \_ -> do
          need [runNetlistTclPath, postRouteCheckpointPath]
          vivadoFromTcl runNetlistTclPath

        -- Bitstream generation
        runBitstreamTclPath %> \path -> do
          writeFileChanged path (mkBitstreamTcl synthesisDir)

        bitstreamPath %> \_ -> do
          need (runBitstreamTclPath : netlistPaths)
          vivadoFromTcl runBitstreamTclPath

        -- Write bitstream to board
        runBoardProgramTclPath %> \path -> do
          writeFileChanged path (mkBoardProgramTcl synthesisDir)

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
        need netlistPaths

      phony (nameBase target <> ":bitstream") $ do
        need [bitstreamPath]

      phony (nameBase target <> ":program") $ do
        need [bitstreamPath, runBoardProgramTclPath]
        vivadoFromTcl runBoardProgramTclPath
