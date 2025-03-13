-- SPDX-FileCopyrightText: 2022 Google LLC
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

import Bittide.Hitl (
  HitlTestGroup (..),
  TestStepResult (..),
  hwTargetRefsFromHitlTestGroup,
 )
import Bittide.Instances.Hitl.Tests (ClashTargetName, hitlTests)
import Clash.DataFiles (tclConnector)
import Clash.Shake.Extra
import Clash.Shake.Flags
import Clash.Shake.Vivado
import Control.Monad (unless, when)
import Control.Monad.Extra (ifM, unlessM, (&&^))
import Data.Foldable (for_)
import Data.Function ((&))
import Data.List (sort, uncons)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Development.Shake
import Development.Shake.Classes
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Syntax (mkName)
import System.Console.ANSI (setSGR)
import System.Directory hiding (doesFileExist)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath
import System.Process (readProcess)

import qualified Clash.Util.Interpolate as I
import qualified Paths.Bittide.Shake as Shake (getDataFileName)
import qualified System.Directory as Directory

{- | Get all files whose changes will trigger an HDL rebuild. Because we lack a
reliable way to determine which files should trigger a rebuild, this function
returns a (very) pessimistic list: all files in the project's directory,
except files ignored by git and files matching patterns in 'ignorePatterns'.
-}
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

{- | File patterns of file we do _not_ want to make trigger a rebuild of HDL
files. Also see 'getWatchFiles'.
-}
ignorePatterns :: [String]
ignorePatterns =
  [ "*.md"
  , ".github"
  , ".reuse"
  , ".vscode"
  , -- Used for synthesis, but not for generating Clash output:
    dataFilesDir </> "**" </> "*.xdc"
  , dataFilesDir </> "**" </> "*.tcl"
  , -- Used for HITL tests
    "bittide-instances/data/openocd/*"
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

{- | Searches for a file called @cabal.project@ It will look for it in the
current working directory. If it can't find it there, it will traverse up
until it finds the file.

The returned path points to the directory containing @cabal.project@. Errors
if it could not find @cabal.project@ anywhere.
-}
findProjectRoot :: (HasCallStack) => IO FilePath
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

-- | Shake target
data Target = Target
  { targetName :: ClashTargetName
  -- ^ TemplateHaskell reference to top entity to synthesize
  , targetHasXdc :: Bool
  -- ^ Whether target has an associated XDC file in 'data/constraints'. An XDC
  -- file implies that a bitstream can be generated.
  , targetHasVio :: Bool
  -- ^ Whether target has one or more VIOs
  , targetTest :: Maybe HitlTestGroup
  -- ^ Whether target has a VIO probe that can be used to run hardware-in-the-
  -- loop tests. Note that this flag, 'targetTest', implies 'targetHasVio'.
  , targetExtraXdc :: [FilePath]
  -- ^ Extra constraints to be sourced. Will be sourced _after_ main XDC.
  , targetExternalHdl :: [TclGlobPattern]
  -- ^ A list of patterns that match the external HDL files that are used by the
  -- instance. Generates tck that utilizes https://www.tcl.tk/man/tcl8.6/TclCmd/glob.htm
  }

defTarget :: ClashTargetName -> Target
defTarget name =
  Target
    { targetName = name
    , targetHasXdc = False
    , targetHasVio = False
    , targetTest = Nothing
    , targetExtraXdc = []
    , targetExternalHdl = []
    }

testTarget :: HitlTestGroup -> Target
testTarget test@(HitlTestGroup{..}) =
  Target
    { targetName = topEntity
    , targetHasXdc = True
    , targetHasVio = True
    , targetTest = Just test
    , targetExtraXdc = extraXdcFiles
    , targetExternalHdl = externalHdl
    }

enforceValidTarget :: Target -> Target
enforceValidTarget target@Target{..}
  | isJust targetTest && not targetHasVio =
      error $
        show targetName
          <> " should have set 'targetHasVio', because"
          <> " the target has a test ('targetTest')."
  | otherwise = target

-- | All synthesizable targets
targets :: [Target]
targets =
  map enforceValidTarget $
    [ defTarget $ mkName "Bittide.Instances.Pnr.Calendar.switchCalendar1k"
    , defTarget $ mkName "Bittide.Instances.Pnr.Calendar.switchCalendar1kReducedPins"
    , defTarget $ mkName "Bittide.Instances.Pnr.Counter.counterReducedPins"
    , defTarget $ mkName "Bittide.Instances.Pnr.ElasticBuffer.elasticBuffer5"
    , defTarget $ mkName "Bittide.Instances.Pnr.ProcessingElement.vexRiscUartHello"
    , defTarget $ mkName "Bittide.Instances.Pnr.ScatterGather.gatherUnit1K"
    , defTarget $ mkName "Bittide.Instances.Pnr.ScatterGather.gatherUnit1KReducedPins"
    , defTarget $ mkName "Bittide.Instances.Pnr.ScatterGather.scatterUnit1K"
    , defTarget $ mkName "Bittide.Instances.Pnr.ScatterGather.scatterUnit1KReducedPins"
    , defTarget $ mkName "Bittide.Instances.Pnr.Si539xSpi.si5391Spi"
    , defTarget $ mkName "Bittide.Instances.Pnr.StabilityChecker.stabilityChecker_3_1M"
    , defTarget $ mkName "Bittide.Instances.Pnr.Synchronizer.safeDffSynchronizer"
    ]
      <> (testTarget <$> Bittide.Instances.Hitl.Tests.hitlTests)

shakeOpts :: ShakeOptions
shakeOpts =
  shakeOptions
    { shakeFiles = buildDir
    , shakeChange = ChangeDigest
    , shakeVersion = "11"
    }

{- | Constructs a 'BoardPart' based on environment variables @SYNTHESIS_BOARD@
or @SYNTHESIS_PART@. Errors if both are set, returns a default (free) part
if neither is set.
-}
getBoardPart :: Action BoardPart
getBoardPart = do
  boardName <- getEnv "SYNTHESIS_BOARD"
  partName <- getEnv "SYNTHESIS_PART"
  case (boardName, partName) of
    (Just b, Nothing) -> pure $ Board b
    (Nothing, Just p) -> pure $ Part p
    (Nothing, Nothing) -> pure $ Part "xcku035-ffva1156-2-e"
    (Just _b, Just _p) ->
      error "Both 'SYNTHESIS_BOARD' and 'SYNTHESIS_PART' are set, unset either and retry"

{- | Inspect DRC and timing report. Throw an error if suspicious strings were
found.
-}
meetsDrcOrError :: FilePath -> FilePath -> FilePath -> IO ()
meetsDrcOrError methodologyPath summaryPath checkpointPath =
  unlessM
    (meetsTiming methodologyPath &&^ meetsTiming summaryPath)
    ( error
        [I.i|
      Design did not meet design rule checks (DRC). Check out the timing summary at:

        #{summaryPath}

      Check out the methodology report at:

        #{methodologyPath}

      You can investigate interactively by opening the latest checkpoint with Vivado:

        vivado #{checkpointPath}

    |]
    )

-- | Newtype used for adding oracle rules for flags to Shake
newtype ForceTestRerun = ForceTestRerun ()
  deriving (Show)
  deriving newtype (Eq, Typeable, Hashable, Binary, NFData)

type instance RuleResult ForceTestRerun = Bool

{- | Defines a Shake build executable for calling Vivado. Like Make, in Shake
you define rules that explain how to build a certain file. For example:

    manifestPath %> ...

means: to build @manifestPath@ I need to do dot-dot-dot. See the README for
an overview of which commands are user-passable (or simply scroll down).

For a fundamental introduction into Shake, read the (lightweight!) paper
introducing it:

  https://ndmitchell.com/downloads/paper-shake_before_building-10_sep_2012.pdf.

Or, see https://shakebuild.com/.
-}
main :: IO ()
main = do
  setCurrentDirectory =<< findProjectRoot

  shakeArgsWith shakeOpts customFlags $ \flags shakeTargets -> pure $ Just $ do
    let
      Options{..} = foldl (&) defaultOptions flags

      rules = do
        _ <- addOracle $ \(ForceTestRerun _) -> return forceTestRerun

        -- 'all' builds all targets defined below
        phony "all" $ do
          for_ targets $ \Target{..} -> do
            need [entityName targetName <> ":synth"]

        (dataFilesDir </> "**") %> \_ -> do
          Stdout out <-
            command
              []
              "cabal"
              [ "sdist"
              , "bittide"
              , "bittide-extra"
              , "bittide-experiments"
              , "bittide-instances"
              , "bittide-tools"
              ]
          command_ [] "mkdir" ["-p", dataFilesDir]
          for_ (filter (((==) (Just '/')) . fmap fst . uncons) $ lines out) $
            \sdist -> do
              (Exit (_ :: ExitCode), Stderr ()) <-
                command
                  []
                  "tar"
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
        for_ targets $ \Target{..} -> do
          let
            -- TODO: Dehardcode these paths. They're currently hardcoded in both the
            --       TCL and here, which smells.
            manifestPath = getManifestLocation clashBuildDir targetName
            synthesisDir = vivadoBuildDir </> show targetName
            checkpointsDir = synthesisDir </> "checkpoints"
            netlistDir = synthesisDir </> "netlist"
            reportDir = synthesisDir </> "reports"
            ilaDataDir = synthesisDir </> "ila-data"

            postSynthCheckpointPath = checkpointsDir </> "post_synth.dcp"
            postPlaceCheckpointPath = checkpointsDir </> "post_place.dcp"
            postRouteCheckpointPath = checkpointsDir </> "post_route.dcp"

            netlistPaths =
              [ netlistDir </> "netlist.v"
              , netlistDir </> "netlist.xdc"
              ]
            bitstreamPath = synthesisDir </> "bitstream.bit"
            probesFilePath = synthesisDir </> "probes.ltx"
            testExitCodePath = synthesisDir </> "test_exit_code"

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
            (postSynthCheckpointPath : synthReportsPaths) |%> \_ -> do
              -- XXX: Will not re-run if _dependencies_ mentioned in 'manifestPath'
              --      change. This is only relevant in designs with multiple
              --      binders with 'Synthesize' pragmas, which we currently do
              --      not have. Ideally we would parse the manifest file and
              --      also depend on the dependencies' manifest files, etc.
              connector <- liftIO tclConnector
              need [manifestPath, connector]
              let
                xdcNames = entityName targetName <> ".xdc" : targetExtraXdc
                xdcPaths = map ((dataFilesDir </> "constraints") </>) xdcNames
              constraints <-
                if targetHasXdc
                  then do
                    need xdcPaths
                    pure xdcPaths
                  else pure []

              synthesisPart <- getBoardPart
              locatedManifest <- decodeLocatedManifest manifestPath

              liftIO $
                runSynthesis
                  synthesisDir -- Output directory for Vivado
                  False -- Out of context run
                  synthesisPart -- Part we're synthesizing for
                  constraints -- List of filenames with constraints
                  targetExternalHdl -- List of external HDL files to be included in synthesis
                  locatedManifest
                  connector -- Path to tclConnector script

            -- Routing + netlist generation
            ( postPlaceCheckpointPath
                : postRouteCheckpointPath
                : routeReportsPaths
                  <> netlistPaths
              )
              |%> \_ -> do
                need [postSynthCheckpointPath]
                liftIO $ runPlaceAndRoute synthesisDir

                -- Design should meet design rule checks (DRC).
                liftIO $
                  unlessM
                    (meetsTiming postRouteMethodologyPath &&^ meetsTiming postRouteTimingSummaryPath)
                    ( error
                        [I.i|
                  Design did not meet design rule checks (DRC). Check out the timing summary at:

                    #{postRouteTimingSummaryPath}

                  Check out the methodology report at:

                    #{postRouteMethodologyPath}

                  You can investigate interactively by opening the latest checkpoint with Vivado:

                    vivado #{postRouteCheckpointPath}

                  You can recreate the files (locally) using:

                    shake #{targetName}:pnr

                |]
                    )

                -- Design should meet timing post routing. Note that this is not a
                -- requirement after synthesis as many of the optimizations only follow
                -- after.
                liftIO $
                  unlessM
                    (meetsTiming postRouteTimingSummaryPath)
                    ( error
                        [I.i|
                  Design did not meet timing. Check out the timing summary at:

                    #{postRouteTimingSummaryPath}

                  Alternatively, check out the full report:

                    #{postRouteTimingPath}

                  You can investigate interactively by opening the latest checkpoint with Vivado:

                    vivado #{postRouteCheckpointPath}

                  You can recreate the files (locally) using:

                    shake #{targetName}:pnr

                |]
                    )

            -- Bitstream generation
            bitstreamPath %> \_ -> do
              need [postRouteCheckpointPath]
              liftIO $ runBitstreamGen synthesisDir

            -- Probes file generation
            probesFilePath %> \_ -> do
              need [bitstreamPath]
              liftIO $ runProbesFileGen synthesisDir

            -- Run hardware test
            testExitCodePath %> \path -> do
              forceRerun <- askOracle $ ForceTestRerun ()
              when forceRerun alwaysRerun
              command_ [Cwd "firmware-binaries"] "cargo" ["build", "--release"]
              command_ [Cwd "firmware-binaries"] "cargo" ["build"]
              need [entityName targetName <> ":program"]
              url <- getEnvWithDefault "localhost:3121" "HW_SERVER_URL"
              exitCode <-
                liftIO $
                  runHitlTest (fromJust targetTest) url probesFilePath ilaDataDir
              writeFileChanged path (show exitCode)

              shortenNamesPy <-
                liftIO $
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
              when targetHasVio $ need [probesFilePath]
              need [bitstreamPath]

            -- Write bitstream to hardware target(s)
            phony (entityName targetName <> ":program") $ do
              -- The Shake target ':program' does not depend on a respective bitstream and
              -- probes file being build. The programming itself does, so error if either
              -- doesn't exist.
              liftIO $
                unlessM
                  (Directory.doesFileExist bitstreamPath)
                  (error $ "Could not program device, missing bitstream file: " <> bitstreamPath)
              when targetHasVio $
                liftIO $
                  unlessM
                    ((Directory.doesFileExist probesFilePath))
                    (error $ "Could not program device, missing probes file: " <> probesFilePath)
              let hwTRefs =
                    hwTargetRefsFromHitlTestGroup $
                      fromMaybe
                        ( error $
                            "Asked to program target "
                              ++ show targetName
                              ++ " while the "
                                <> "hardware targets to program could not be found as this target does not "
                                <> "have a HITL test associated with it."
                        )
                        targetTest
              url <- getEnvWithDefault "localhost:3121" "HW_SERVER_URL"
              liftIO $ programBitstream synthesisDir hwTRefs url targetHasVio

            when (isJust targetTest) $ do
              phony (entityName targetName <> ":test") $ do
                need [testExitCodePath]
                exitCode <- read <$> readFile' testExitCodePath
                when (isJust (mPostProc =<< targetTest)) $ do
                  res <- liftIO $ (fromJust $ mPostProc =<< targetTest) ilaDataDir exitCode
                  checkTestStep res
                unless (exitCode == ExitSuccess) $ do
                  liftIO $ exitWith exitCode

              when (isJust (mPostProc =<< targetTest)) $ do
                phony (entityName targetName <> ":post-process") $ do
                  need [testExitCodePath]
                  exitCode <- read <$> readFile' testExitCodePath
                  res <- liftIO $ (fromJust (mPostProc =<< targetTest)) ilaDataDir exitCode
                  checkTestStep res

    if null shakeTargets
      then rules
      else want shakeTargets >> withoutActions rules

checkTestStep :: (MonadFail m, HasCallStack) => TestStepResult a -> m a
checkTestStep res = case res of
  TestStepFailure err -> error err
  TestStepSuccess x -> return x
