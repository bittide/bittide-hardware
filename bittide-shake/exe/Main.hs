-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Prelude

import Clash.Shake.Extra
import Clash.Shake.Flags
import Clash.Shake.Vivado
import Control.Applicative (liftA2)
import Control.Monad (forM_, unless, when)
import Control.Monad.Extra (ifM, unlessM)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.List (isPrefixOf, sort, uncons)
import Data.Maybe (fromJust, isJust)
import Development.Shake
import Development.Shake.Classes
import GHC.Stack (HasCallStack)
import System.Console.ANSI (setSGR)
import System.Directory hiding (doesFileExist)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath
import System.FilePath.Glob (glob)
import System.Process (readProcess, callProcess)
import Test.Tasty.HUnit (Assertion)

import qualified Clash.Util.Interpolate as I
import qualified Paths.Bittide.Shake as Shake (getDataFileName)
import qualified System.Directory as Directory

-- | Get all files whose changes will trigger an HDL rebuild. Because we lack a
-- reliable way to determine which files should trigger a rebuild, this function
-- returns a (very) pessimistic list: all files in the project's directory,
-- except files ignored by git and files matching patterns in 'ignorePatterns'.
getWatchFiles :: IO [String]
getWatchFiles = do
  getWatchFilesPy <-
    Shake.getDataFileName ("data" </> "scripts" </> "get_watch_files.py")
  lines <$> readProcess getWatchFilesPy ignorePatterns ""

-- | Call 'need' on output of 'getWatchFiles'
needWatchFiles :: Action ()
needWatchFiles = do
  need [watchFilesPath]
  need =<< liftIO (lines <$> readFile watchFilesPath)

-- | File patterns of file we do _not_ want to make trigger a rebuild of HDL
-- files. Also see 'getWatchFiles'.
ignorePatterns :: [String]
ignorePatterns =
  [ "*.md"
  , ".github"
  , ".reuse"
  , ".vscode"

  -- Used for synthesis, but not for generating Clash output:
  , dataFilesDir </> "**" </> "*.xdc"
  , dataFilesDir </> "**" </> "*.tcl"

  -- Used for HITL tests
  , "bittide-instances/data/openocd/*"
  , "bittide-instances/data/picocom/*"
  , "bittide-instances/data/gdb/*"
  ]

-- | Build directory for Shake/Vivado/Cargo (not Cabal, it ignores builddir settings)
buildDir :: FilePath
buildDir = "_build"

-- | Clash HDL output directory
clashBuildDir :: FilePath
clashBuildDir = buildDir </> "clash"

-- | Directory for Vivado input + output files
vivadoBuildDir :: FilePath
vivadoBuildDir = buildDir </> "vivado"

hitlBuildDir :: FilePath
hitlBuildDir = buildDir </> "hitl"

dataFilesDir :: FilePath
dataFilesDir = buildDir </> "data"

-- | List of files to watch for Cabal/Cargo cache invalidation
watchFilesPath :: FilePath
watchFilesPath = buildDir </> "watch_files.txt"

-- | Build and run the executable for post processing of ILA data
doPostProcessing :: String -> FilePath -> ExitCode -> Assertion
doPostProcessing postProcessMain ilaDir testExitCode = do
  callProcess "cabal" ["build", postProcessMain]
  callProcess "cabal" ["run", postProcessMain, ilaDir, show testExitCode]

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

data Target = Target
  { -- | TemplateHaskell reference to top entity to synthesize
    targetName :: TargetName

    -- | Whether target has an associated XDC file in 'data/constraints'. An XDC
    -- file implies that a bitstream can be generated.
  , targetHasXdc :: Bool

    -- | Whether target has one or more VIOs
  , targetHasVio :: Bool

    -- | Whether target has a VIO probe that can be used to run hardware-in-the-
    -- loop tests. Note that this flag, 'targetHasTest', implies 'targetHasVio'.
  , targetHasTest :: Bool

    -- | Name of the executable for post processing of ILA CSV data, or Nothing
    -- if it has none.
  , targetPostProcess :: Maybe String

    -- | Extra constraints to be sourced. Will be sourced _after_ main XDC.
  , targetExtraXdc :: [FilePath]
  }


defTarget :: TargetName -> Target
defTarget name = Target
  { targetName = name
  , targetHasXdc = False
  , targetHasVio = False
  , targetHasTest = False
  , targetPostProcess = Nothing
  , targetExtraXdc = []
  }

testTarget :: TargetName -> Target
testTarget name = Target
  { targetName = name
  , targetHasXdc = True
  , targetHasVio = True
  , targetHasTest = True
  , targetPostProcess = Nothing
  , targetExtraXdc = []
  }

enforceValidTarget :: Target -> Target
enforceValidTarget target@Target{..}
  | targetHasTest && not targetHasVio =
      error $ show targetName <> " should have set 'targetHasVio', because " <>
                                 "'targetHasTest' was asserted."
  | otherwise = target


-- | All synthesizable targets
targets :: [Target]
targets = map enforceValidTarget
  [ defTarget "Bittide.Instances.Pnr.Calendar.switchCalendar1k"
  , defTarget "Bittide.Instances.Pnr.Calendar.switchCalendar1kReducedPins"
  , defTarget "Bittide.Instances.Pnr.ClockControl.callisto3"
  , defTarget "Bittide.Instances.Pnr.Counter.counterReducedPins"
  , defTarget "Bittide.Instances.Pnr.ElasticBuffer.elasticBuffer5"
  , (defTarget "Bittide.Instances.Pnr.I2C.i2cTest")
      {targetHasXdc = True, targetHasVio = True}
  , defTarget "Bittide.Instances.Pnr.ScatterGather.gatherUnit1K"
  , defTarget "Bittide.Instances.Pnr.ScatterGather.gatherUnit1KReducedPins"
  , defTarget "Bittide.Instances.Pnr.ScatterGather.scatterUnit1K"
  , defTarget "Bittide.Instances.Pnr.ScatterGather.scatterUnit1KReducedPins"
  , defTarget "Bittide.Instances.Pnr.Si539xSpi.callistoSpi"
  , defTarget "Bittide.Instances.Pnr.Si539xSpi.si5391Spi"
  , defTarget "Bittide.Instances.Pnr.StabilityChecker.stabilityChecker_3_1M"
  , defTarget "Bittide.Instances.Pnr.Synchronizer.safeDffSynchronizer"

  , (testTarget "Bittide.Instances.Hitl.BoardTest.boardTestExtended")
      {targetPostProcess = Just "post-board-test-extended"}
  , testTarget "Bittide.Instances.Hitl.BoardTest.boardTestSimple"
  , testTarget "Bittide.Instances.Hitl.FincFdec.fincFdecTests"
  , testTarget "Bittide.Instances.Hitl.FullMeshHwCc.fullMeshHwCcTest"
  , testTarget "Bittide.Instances.Hitl.FullMeshHwCc.fullMeshHwCcWithRiscvTest"
  , testTarget "Bittide.Instances.Hitl.FullMeshSwCc.fullMeshSwCcTest"
  , testTarget "Bittide.Instances.Hitl.HwCcTopologies.hwCcTopologyTest"
  , testTarget "Bittide.Instances.Hitl.LinkConfiguration.linkConfigurationTest"
  , testTarget "Bittide.Instances.Hitl.SyncInSyncOut.syncInSyncOut"
  , testTarget "Bittide.Instances.Hitl.Tcl.ExtraProbes.extraProbesTest"
  , testTarget "Bittide.Instances.Hitl.Transceivers.transceiversUpTest"
  , (testTarget "Bittide.Instances.Hitl.VexRiscv.vexRiscvTest")
      { targetPostProcess = Just "post-vex-riscv-test"
      , targetExtraXdc = ["jtag.xdc"]
      }
  ]

shakeOpts :: ShakeOptions
shakeOpts = shakeOptions
  { shakeFiles = buildDir
  , shakeChange = ChangeDigest
  , shakeVersion = "11"
  }

-- | Run Vivado on given TCL script. Can collect the ExitCode.
vivadoFromTcl :: CmdResult r => FilePath -> Action r
vivadoFromTcl tclPath =
  command
    [AddEnv "XILINX_LOCAL_USER_DATA" "no"] -- Prevents multiprocessing issues
    "vivado"
    ["-mode", "batch", "-source", tclPath]

-- | Run Vivado on given TCL script
vivadoFromTcl_ :: FilePath -> Action ()
vivadoFromTcl_ tclPath =
  command_
    [AddEnv "XILINX_LOCAL_USER_DATA" "no"] -- Prevents multiprocessing issues
    "vivado"
    ["-mode", "batch", "-source", tclPath, "-notrace"]

-- | Constructs a 'BoardPart' based on environment variables @SYNTHESIS_BOARD@
-- or @SYNTHESIS_PART@. Errors if both are set, returns a default (free) part
-- if neither is set.
getBoardPart :: Action BoardPart
getBoardPart = do
  boardName <- getEnv "SYNTHESIS_BOARD"
  partName <- getEnv "SYNTHESIS_PART"
  case (boardName, partName) of
    (Just b,  Nothing) -> pure $ Board b
    (Nothing, Just p)  -> pure $ Part p
    (Nothing, Nothing) -> pure $ Part "xcku035-ffva1156-2-e"
    (Just _b,  Just _p)  ->
      error "Both 'SYNTHESIS_BOARD' and 'SYNTHESIS_PART' are set, unset either and retry"

-- | Inspect DRC and timing report. Throw an error if suspicious strings were
-- found.
meetsDrcOrError :: FilePath -> FilePath -> FilePath -> IO ()
meetsDrcOrError methodologyPath summaryPath checkpointPath =
  unlessM
    (liftA2 (&&) (meetsTiming methodologyPath) (meetsTiming summaryPath))
    (error [I.i|
      Design did not meet design rule checks (DRC). Check out the timing summary at:

        #{summaryPath}

      Check out the methodology report at:

        #{methodologyPath}

      You can investigate interactively by opening the latest checkpoint with Vivado:

        vivado #{checkpointPath}

    |])

-- | Newtype used for adding oracle rules for flags to Shake
newtype HardwareTargetsFlag = HardwareTargetsFlag ()
  deriving (Show)
  deriving newtype (Eq, Typeable, Hashable, Binary, NFData)
type instance RuleResult HardwareTargetsFlag = HardwareTargets

newtype ForceTestRerun = ForceTestRerun ()
  deriving (Show)
  deriving newtype (Eq, Typeable, Hashable, Binary, NFData)
type instance RuleResult ForceTestRerun = Bool

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
  setCurrentDirectory =<< findProjectRoot

  shakeArgsWith shakeOpts customFlags $ \flags shakeTargets -> pure $ Just $ do

    let
      Options{..} = foldl (&) defaultOptions flags

      rules = do
        _ <- addOracle $ \(ForceTestRerun _) -> return forceTestRerun
        _ <- addOracle $ \(HardwareTargetsFlag _) -> return hardwareTargets

        -- 'all' builds all targets defined below
        phony "all" $ do
          for_ targets $ \Target{..} -> do
            need [entityName targetName <> ":synth"]

        (hitlBuildDir </> "*.yml") %> \path -> do
          needWatchFiles
          let entity = takeFileName (dropExtension path)
          command_ [] "cabal"
            [ "run", "--"
            , "bittide-tools:hitl-config-gen"
            , "write", entity
            ]

        (dataFilesDir </> "**") %> \_ -> do
          Stdout out <-
            command [] "cabal"
              [ "sdist"
              , "bittide"
              , "bittide-extra"
              , "bittide-experiments"
              , "bittide-instances"
              , "bittide-tools"
              ]
          command_ [] "mkdir" ["-p", dataFilesDir ]
          for_ (filter (((==) (Just '/')) . fmap fst . uncons) $ lines out)
            $ \sdist -> do
              (Exit (_ :: ExitCode), Stderr ()) <- command [] "tar"
                [ "--strip-components=2"
                , "--overwrite"
                , "-C"
                , dataFilesDir
                , "-xf"
                , sdist
                , takeBaseName (takeBaseName sdist) </> "data"
                ]
              return ()

        -- Files used for cache invalidation
        watchFilesPath %> \_ -> do
          alwaysRerun
          watchFiles <- sort <$> liftIO getWatchFiles
          writeFileChanged watchFilesPath (unlines watchFiles)

        -- For each target, generate a user callable command (PHONY). Run with
        -- '--help' to list them.
        for_ targets $ \Target{..}-> do
          let
            -- TODO: Dehardcode these paths. They're currently hardcoded in both the
            --       TCL and here, which smells.
            manifestPath = getManifestLocation clashBuildDir targetName
            synthesisDir = vivadoBuildDir </> targetName
            checkpointsDir = synthesisDir </> "checkpoints"
            netlistDir = synthesisDir </> "netlist"
            reportDir = synthesisDir </> "reports"
            ilaDir = synthesisDir </> "ila-data"

            runSynthTclPath         = synthesisDir </> "run_synth.tcl"
            runPlaceAndRouteTclPath = synthesisDir </> "run_place_and_route.tcl"
            runBitstreamTclPath     = synthesisDir </> "run_bitstream.tcl"
            runProbesGenTclPath     = synthesisDir </> "run_probes_gen.tcl"
            runBoardProgramTclPath  = synthesisDir </> "run_board_program.tcl"
            runHardwareTestTclPath  = synthesisDir </> "run_hardware_test.tcl"

            postSynthCheckpointPath = checkpointsDir </> "post_synth.dcp"
            postPlaceCheckpointPath = checkpointsDir </> "post_place.dcp"
            postRouteCheckpointPath = checkpointsDir </> "post_route.dcp"

            netlistPaths =
              [ netlistDir </> "netlist.v"
              , netlistDir </> "netlist.xdc"
              ]
            bitstreamPath = synthesisDir </> "bitstream.bit"
            probesPath = synthesisDir </> "probes.ltx"
            testExitCodePath = synthesisDir </> "test_exit_code"
            hitlConfigPath = hitlBuildDir </> targetName <> ".yml"

            postRouteMethodologyPath = reportDir </> "post_route_methodology.rpt"
            postRouteTimingSummaryPath = reportDir </> "post_route_timing_summary.rpt"
            postRouteTimingPath = reportDir </> "post_route_timing.rpt"

            synthReportsPaths = [reportDir </> "post_synth_timing_summary.rpt"]
            routeReportsPaths =
              [ reportDir </> "post_route_clock_util.rpt"
              , reportDir </> "post_route_drc.rpt"
              , reportDir </> "post_route_methodology.rpt"
              , reportDir </> "post_route_power.rpt"
              , reportDir </> "post_route_timing_summary.rpt"
              , reportDir </> "post_route_timing.rpt"
              , reportDir </> "post_route_util.rpt"
              ]

          withoutTargets $ do
            manifestPath %> \path -> do
              needWatchFiles

              -- We build all rust binaries in "firmware-binaries". They are required to
              -- build bittide-instance because we have instances that includes a binaries.
              command_ [Cwd "firmware-binaries"] "cargo" ["build", "--release"]
              command_ [Cwd "firmware-binaries"] "cargo" ["build"]

              -- XXX: Cabal/GHC doesn't know about files produced by cargo, and
              --      will therefore fail to invalidate caches. While there are
              --      ways to tell Cabal/GHC to depend on these files, they are
              --      known to be broken in our tool versions. This workaround
              --      removes all build artifacts _except_ for "bittide-shake".
              --
              --      See: https://github.com/haskell/cabal/issues/4746
              --
              --      We need to manually remove build artifacts, because Cabal
              --      does not support per package/component cleans:
              --
              --      See: https://github.com/haskell/cabal/issues/7506
              --
              ci <- getEnvWithDefault "false" "CI"
              unless (ci == "true" || ci == "false") $ do
                error $ "Environment variable 'CI' must be either 'true' or 'false', but it is: " <> ci
              when (ci == "false") $ do
                buildDirs <- liftIO (glob "dist-newstyle/build/*/ghc-*/*")
                forM_ buildDirs $ \dir -> do
                  let fileName = takeFileName dir
                  unless ("bittide-shake" `isPrefixOf` fileName) $
                    command_ [] "rm" ["-rf", dir]

              -- Generate RTL
              let
                (buildTool, buildToolArgs) =
                  defaultClashCmd clashBuildDir targetName
              command_ [] buildTool buildToolArgs

              -- Clash messes up ANSI escape codes, leaving the rest of the terminal
              -- printed in bold text. Reset manually:
              liftIO (setSGR [])

              produces [path]

            -- Synthesis
            runSynthTclPath %> \path -> do
              let
                xdcNames = entityName targetName <> ".xdc" : targetExtraXdc
                xdcPaths = map ((dataFilesDir </> "constraints") </>) xdcNames
              constraints <-
                if targetHasXdc then do
                  need xdcPaths
                  pure xdcPaths
                else
                  pure []

              synthesisPart <- getBoardPart
              locatedManifest <- decodeLocatedManifest manifestPath

              tcl <-
                mkSynthesisTcl
                  synthesisDir            -- Output directory for Vivado
                  False                   -- Out of context run
                  synthesisPart           -- Part we're synthesizing for
                  constraints             -- List of filenames with constraints
                  locatedManifest

              writeFileChanged path tcl

            (postSynthCheckpointPath : synthReportsPaths) |%> \_ -> do
              -- XXX: Will not re-run if _dependencies_ mentioned in 'manifestPath'
              --      change. This is only relevant in designs with multiple
              --      binders with 'Synthesize' pragmas, which we currently do
              --      not have. Ideally we would parse the manifest file and
              --      also depend on the dependencies' manifest files, etc.
              need [runSynthTclPath, manifestPath]
              vivadoFromTcl_ runSynthTclPath

            -- Routing + netlist generation
            runPlaceAndRouteTclPath %> \path -> do
              writeFileChanged path (mkPlaceAndRouteTcl synthesisDir)

            (  postPlaceCheckpointPath
             : postRouteCheckpointPath
             : routeReportsPaths
             <> netlistPaths
             ) |%> \_ -> do
              need [runPlaceAndRouteTclPath, postSynthCheckpointPath]
              vivadoFromTcl_ runPlaceAndRouteTclPath

              -- Design should meet design rule checks (DRC).
              liftIO $ unlessM
                ( liftA2
                    (&&)
                    (meetsTiming postRouteMethodologyPath)
                    (meetsTiming postRouteTimingSummaryPath)
                )
                (error [I.i|
                  Design did not meet design rule checks (DRC). Check out the timing summary at:

                    #{postRouteTimingSummaryPath}

                  Check out the methodology report at:

                    #{postRouteMethodologyPath}

                  You can investigate interactively by opening the latest checkpoint with Vivado:

                    vivado #{postRouteCheckpointPath}

                  You can recreate the files (locally) using:

                    shake #{targetName}:pnr

                |])

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

                  You can recreate the files (locally) using:

                    shake #{targetName}:pnr

                |])

            -- Bitstream generation
            runBitstreamTclPath %> \path -> do
              writeFileChanged path (mkBitstreamTcl synthesisDir)

            bitstreamPath %> \_ -> do
              need [runBitstreamTclPath, postRouteCheckpointPath]
              vivadoFromTcl_ runBitstreamTclPath

            -- Probes file generation
            runProbesGenTclPath %> \path -> do
              writeFileChanged path (mkProbesGenTcl synthesisDir)

            probesPath %> \_ -> do
              need [runProbesGenTclPath, bitstreamPath]
              vivadoFromTcl_ runProbesGenTclPath

            -- Write bitstream to board
            runBoardProgramTclPath %> \path -> do
              hwTargets <- askOracle $ HardwareTargetsFlag ()
              url <- getEnvWithDefault "localhost:3121" "HW_SERVER_URL"
              boardProgramTcl <-
                liftIO $ mkBoardProgramTcl synthesisDir hwTargets url targetHasVio
              writeFileChanged path boardProgramTcl

            -- Run hardware test
            runHardwareTestTclPath %> \path -> do
              hwTargets <- askOracle $ HardwareTargetsFlag ()
              need [hitlConfigPath]
              forceRerun <- askOracle $ ForceTestRerun ()
              when forceRerun alwaysRerun
              url <- getEnvWithDefault "localhost:3121" "HW_SERVER_URL"
              hardwareTestTcl <-
                liftIO $ mkHardwareTestTcl hitlConfigPath synthesisDir hwTargets url ilaDir
              writeFileChanged path hardwareTestTcl

            testExitCodePath %> \path -> do
              forceRerun <- askOracle $ ForceTestRerun ()
              when forceRerun alwaysRerun
              need
                [ runBoardProgramTclPath
                , runHardwareTestTclPath
                , bitstreamPath
                , probesPath
                , hitlConfigPath
                ]
              vivadoFromTcl_ runBoardProgramTclPath
              exitCode <- vivadoFromTcl @ExitCode runHardwareTestTclPath
              writeFileChanged path $ show exitCode

              shortenNamesPy <- liftIO $
                Shake.getDataFileName ("data" </> "scripts" </> "shorten_names.py")
              command_ [] "python3" [shortenNamesPy]


          -- User friendly target names
          phony (entityName targetName <> ":hdl") $ do
            need [manifestPath]

          phony (entityName targetName <> ":synth") $ do
            need [postSynthCheckpointPath]

          phony (entityName targetName <> ":pnr") $ do
            need [postRouteCheckpointPath]

          when targetHasXdc $ do
            phony (entityName targetName <> ":bitstream") $ do
              when targetHasVio $ need [probesPath]
              need [bitstreamPath]

            phony (entityName targetName <> ":program") $ do
              when targetHasVio $ need [probesPath]
              need [runBoardProgramTclPath, bitstreamPath]
              vivadoFromTcl_ runBoardProgramTclPath

            when targetHasTest $ do
              phony (entityName targetName <> ":test") $ do
                need [testExitCodePath]
                exitCode <- read <$> readFile' testExitCodePath
                when (isJust targetPostProcess) $ do
                  liftIO $ doPostProcessing (fromJust targetPostProcess) ilaDir exitCode
                unless (exitCode == ExitSuccess) $ do
                  liftIO $ exitWith exitCode

              when (isJust targetPostProcess) $ do
                phony (entityName targetName <> ":post-process") $ do
                  need [testExitCodePath]
                  exitCode <- read <$> readFile' testExitCodePath
                  liftIO $ doPostProcessing (fromJust targetPostProcess) ilaDir exitCode


    if null shakeTargets then
      rules
    else
      want shakeTargets >> withoutActions rules
