-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}

{- | The standard HITL driver, driving a test purely through the HITL VIO and
collecting any ILA data afterwards. This is the behavior that used to be built
into the test runner for tests that did not supply a custom driver.

Designs driven by this driver must instantiate the HITL VIO (see
'Bittide.Hitl.hitlVio'). The driver:

  1. Verifies the VIO (and any ILAs) are present and correctly shaped.
  2. Sets the test parameter on the @probe_test_data@ probe (if any).
  3. Arms all ILAs.
  4. Asserts @probe_test_start@.
  5. Polls @probe_test_done@ / @probe_test_success@ until done or timeout.
  6. Uploads and writes out ILA data.
  7. Deasserts @probe_test_start@.

All Vivado interaction happens inside a single 'WithVivado' call.
-}
module Bittide.Instances.Hitl.Driver.Default (
  defaultVivadoDriver,

  -- * Building blocks (also usable by custom drivers)
  verifyHitlVio,
  verifyHwIlas,
  waitTestCaseEnd,
  pollTestDone,
  getCurrentIlaShortName,
) where

import Prelude

import Bittide.Hitl
import Bittide.Instances.Hitl.Utils.Vivado

import Clash.Prelude (Natural)

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (isSuffixOf)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.String.Interpolate (i, __i)
import System.Clock (Clock (Monotonic), TimeSpec, diffTimeSpec, getTime, toNanoSecs)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Vivado (VivadoHandle, execPrint_)
import Vivado.Tcl
import Vivado.VivadoM (askVivado)
import "extra" Data.List.Extra (split, (!?))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

{- | The standard VIO/ILA driver. Drives the test purely through the HITL VIO
and collects ILA data afterwards.
-}
defaultVivadoDriver :: HitlDriver
defaultVivadoDriver env@HitlDriverEnv{withVivado} = withVivado $ do
  v <- askVivado
  liftIO $ runDefaultDriver v env

-- | The actual VIO/ILA lifecycle, run against a connected Vivado session.
runDefaultDriver :: VivadoHandle -> HitlDriverEnv -> IO ExitCode
runDefaultDriver v env = do
  -- 'env.targets' carries synthetic 'HwTarget' strings (the hardware server URL
  -- and FPGA id, with a placeholder vendor). Vivado will not accept those as
  -- hardware targets, so resolve them against the targets actually hosted on the
  -- connected hardware server, matching by FPGA id.
  resolvedTargets <- resolveHwTargets v env.targets
  let
    name = env.testName
    testData = resolvedTargets
    probesFilePath = env.probesFilePath
    ilaDataDir = env.ilaDataDir

    -- Map each FPGA ID to the (value, bitSize) test parameter for that target.
    paramByFpgaId :: Map FpgaId (Natural, Natural)
    paramByFpgaId =
      Map.fromList
        [(idFromHwT hwT, (val, sz)) | (hwT, val, sz) <- env.parameterData]

    paramFor :: HwTarget -> (Natural, Natural)
    paramFor hwT =
      fromMaybe
        (error $ "No test parameter found for hardware target " <> prettyShow hwT)
        (Map.lookup (idFromHwT hwT) paramByFpgaId)

  -- Verify ILAs are present and well-shaped before using them.
  case testData of
    ((hwT0, _) : _) -> openHwTarget v hwT0 >> verifyHwIlas v
    [] -> pure ()

  -- Pre-test setup: verify the VIO, set the parameter, arm ILAs.
  forM_ testData $ \(hwT, _deviceInfo) -> do
    let (paramValue, paramBitSize) = paramFor hwT
    openHwTarget v hwT
    execCmd_ v "set_property" ["PROBES.FILE", embrace probesFilePath, "[current_hw_device]"]
    refresh_hw_device v []
    verifyHitlVio v paramBitSize

    execCmd_ v "set_property" ["OUTPUT_VALUE", "0", getProbeTestStartTcl]
    commit_hw_vio v ["[get_hw_vios]"]
    refresh_hw_vio v ["[get_hw_vios]"]
    done <- execCmd v "get_property" ["INPUT_VALUE", getProbeTestDoneTcl]
    when (done /= "0") $
      error $
        "Hardware target '"
          <> prettyShow hwT
          <> "' asserted its HITL VIO probe done before the test was started."
    unless (paramBitSize == 0) $ do
      hexWidth <- execCmd v "expr" [embrace ("(3 + " <> show paramBitSize <> ")/4")]
      vioValue <-
        execCmd
          v
          "format"
          ["%0" <> hexWidth <> "llX " <> show paramValue]
      putStrLn $ "Setting probe_test_data to " <> vioValue <> "..."
      execCmd_ v "set_property" ["OUTPUT_VALUE", vioValue, getProbeTestDataTcl]

    -- Activate the trigger for each ILA.
    putStrLn "Verifying ILAs..."
    ilas <- get_hw_ilas v []
    unless (null ilas) $
      putStrLn "Configuring and arming ILAs..."

    forM_ ilas $ \ila -> do
      _ <- current_hw_ila v [show ila]

      -- Set trigger probe (active high boolean)
      let triggerProbe = "[get_hw_probes -of_objects [current_hw_ila] */trigger*]"
      execCmd_ v "set_property" ["trigger_compare_value", "eq1'b1", triggerProbe]

      -- Enable capture control and set capture probe (active high boolean)
      execCmd_ v "set_property" ["control.capture_mode", "BASIC", "[current_hw_ila]"]
      let captureProbe = "[get_hw_probes -of_objects [current_hw_ila] */capture*]"
      execCmd_ v "set_property" ["capture_compare_value", "eq1'b1", captureProbe]

      -- Set the trigger position
      execCmd_ v "set_property" ["control.trigger_position", "0", "[current_hw_ila]"]

      run_hw_ila v ["[current_hw_ila]"]

    -- Deassert HitlVio start probe
    execCmd_ v "set_property" ["OUTPUT_VALUE", "0", getProbeTestStartTcl]
    commit_hw_vio v ["[get_hw_vios]"]

  -- Assert HitlVio start probe for each target
  forM_ testData $ \(hwT, _deviceInfo) -> do
    openHwTarget v hwT
    execCmd_ v "set_property" ["PROBES.FILE", embrace probesFilePath, "[current_hw_device]"]
    refresh_hw_device v []
    execCmd_ v "set_property" ["OUTPUT_VALUE", "1", getProbeTestStartTcl]
    commit_hw_vio v ["[get_hw_vios]"]
    putStrLn $ "Started test case for hardware target " <> prettyShow hwT <> "."

  putStrLn $ "Waiting for test case '" <> name <> "' to end..."
  testCaseExitCode <- waitTestCaseEnd v name testData probesFilePath

  -- Save captured ILA data (if any).
  putStrLn "Saving captured ILA data (if relevant)..."
  forM_ testData $ \(hwT, _) -> do
    openHwTarget v hwT
    execCmd_ v "set_property" ["PROBES.FILE", embrace probesFilePath, "[current_hw_device]"]
    refresh_hw_device v ["-quiet"]
    ilas <- get_hw_ilas v []
    let dir = ilaDataDir </> name </> prettyShow hwT
    unless (null ilas) $ do
      putStrLn $ "Saving captured ILA data to: " <> dir
      createDirectoryIfMissing True dir
    usedShortNamesVar <- newMVar mempty
    forM_ ilas $ \ila -> do
      _ <- current_hw_ila v [show ila]
      ilaShortName0 <- getCurrentIlaShortName v
      ilaShortName <- mkUniqShortName usedShortNamesVar ilaShortName0 (fromHwIla ila)
      execCmd_ v "current_hw_ila_data" ["[upload_hw_ila_data [current_hw_ila]]"]
      -- Legacy CSV excludes radix information
      execCmd_ v "write_hw_ila_data" ["-force", "-legacy_csv_file " <> embrace (dir </> ilaShortName)]
      execCmd_ v "write_hw_ila_data" ["-force", "-vcd_file " <> embrace (dir </> ilaShortName)]

  -- Deassert all start probes.
  forM_ testData $ \(hwT, _) -> do
    openHwTarget v hwT
    execCmd_ v "set_property" ["PROBES.FILE", embrace probesFilePath, "[current_hw_device]"]
    refresh_hw_device v []
    execCmd_ v "set_property" ["OUTPUT_VALUE", "0", getProbeTestStartTcl]
    commit_hw_vio v ["[get_hw_vios]"]

  pure testCaseExitCode

-- * VIO / ILA verification

data VioProbeInfo = VioProbeInfo
  { probeName :: String
  , probeType :: String
  , probeWidth :: String
  }

{- | Verifies whether the bitstream programmed on the current hardware target
includes a VIO IP core that is configured as required by this HITL framework.
See `Bittide.Hitl` for details.

Make sure that the `PROBES.FILE` property is set for the `current_hw_device`
and that `refresh_hw_device` has been run afterwards.
-}
verifyHitlVio :: VivadoHandle -> Natural -> IO ()
verifyHitlVio v paramBitSize = do
  vioProbes <- get_hw_probes v ["-of_objects [get_hw_vios]", "*vioHitlt/*"]
  let unexpectedProbes =
        [ show probe
        | probe <- vioProbes
        , not (any (`isSuffixOf` show probe) requiredProbeSimpleNames)
        ]
       where
        requiredProbeSimpleNames = map (last . split (== '/') . probeName) requiredProbes
  unless (null unexpectedProbes) $ do
    putStrLn "[WARNING] Encountered unexpected HITL VIO probes, they will be ignored:"
    mapM_ (putStrLn . ('\t' :)) unexpectedProbes
  mapM_ (`verifyHitlProbe` vioProbes) requiredProbes
 where
  requiredProbes =
    [ VioProbeInfo "*vioHitlt/probe_test_start" "vio_output" "1"
    , VioProbeInfo "*vioHitlt/probe_test_done" "vio_input" "1"
    , VioProbeInfo "*vioHitlt/probe_test_success" "vio_input" "1"
    ]
      <> [ VioProbeInfo "*vioHitlt/probe_test_data" "vio_output" (show paramBitSize)
         | paramBitSize /= 0
         ]
  verifyHitlProbe :: VioProbeInfo -> [HwProbe] -> IO ()
  verifyHitlProbe vpi@VioProbeInfo{} vioProbes = do
    let simpleName = last (split (== '/') vpi.probeName)
    let probe = case filter (('/' : simpleName) `isSuffixOf`) (show <$> vioProbes) of
          [p] -> p
          ps ->
            error $
              "Exactly one probe named '"
                <> vpi.probeName
                <> "' "
                <> "must be present but "
                <> show (length ps)
                <> " were found."
    execCmd_
      v
      "set"
      [ simpleName
      , "[get_hw_probes -of_objects [get_hw_vios] " <> vpi.probeName <> "]"
      ]
    typeProp <- execCmd v "get_property" ["type", "$" <> simpleName]
    unless (typeProp == vpi.probeType) $
      error $
        "Probe '"
          <> probe
          <> "' must have type "
          <> vpi.probeType
          <> " but has '"
          <> typeProp
          <> "'."
    widthProp <- execCmd v "get_property" ["width", "$" <> simpleName]
    unless (widthProp == vpi.probeWidth) $
      error $
        "Probe '" <> probe <> "' must have width " <> vpi.probeWidth <> " but it is " <> widthProp

{- | Observed instances of property CELL_NAME of an hw_ila object include:
- "Bittide_Instances_Hitl_FullMeshSwCc_fullMeshSwCcTest_callistoClockControlWithIla_callistoResult/ilaPlot/ilaPlot"
- "instructionBus/dataBus"

This short name should return "ilaPlot" and "instructionBus" for
those examples respectively. Could be improved, see
https://github.com/bittide/bittide-hardware/issues/530
-}
getCurrentIlaShortName :: VivadoHandle -> IO String
getCurrentIlaShortName v = do
  ilaCellName <- execCmd v "get_property" ["CELL_NAME", "[current_hw_ila]"]
  pure $
    fromMaybe
      (error $ "Determining short name failed for ILA with CELL_NAME " <> ilaCellName)
      (reverse (split (== '/') ilaCellName) !? 1)

{- | Verify hardware ILAs. Verification should be performed before the `HwIla`
objects are used for the first time.
-}
verifyHwIlas :: VivadoHandle -> IO ()
verifyHwIlas v = do
  -- TODO either use or remove the Tcl dictionary
  execPrint_
    v
    [__i|
    \# Create a list of dictionaries where each dictionary corresponds to one ILA.
    \# Each dictionary has the following keys:
    \#   name          : short name of the ILA
    \#   cell_name     : name of the cell the ILA is in
    \#   trigger_probe : name of the trigger probe
    \#   capture_probe : name of the capture probe
    \#   data_probes   : list of names of all other probes
    proc get_ila_dicts {} {
        set ila_dicts {}

        set hw_ilas [get_hw_ilas -quiet]
        set ila_count [llength $hw_ilas]
        if {$ila_count == 0} {
            puts "\nNo ILAs in design"
            return {}
        }

        puts "\nFound $ila_count ILAs:"
        foreach hw_ila $hw_ilas {
            set ila_dict {}

            \# The short name is the name of the module the ILA is in. For example a
            \# cell named `fullMeshSwCcTest/ilaPlot/ila_inst` will give the short
            \# name `ilaPlot`.
            set cell_name [get_property CELL_NAME $hw_ila]
            set before_last [expr [string last / $cell_name] - 1]
            set module_name [string range $cell_name 0 $before_last]
            set after_second_to_last [expr [string last / $module_name] + 1]
            set short_name [string range $cell_name $after_second_to_last $before_last]
            dict set ila_dict name $short_name
            dict set ila_dict cell_name $cell_name

            \# Get trigger probe and verify it conforms with ILA framework
            set trigger_probe [get_hw_probes -of_objects $hw_ila */trigger*]
            set trigger_probe_count [llength $trigger_probe]
            if {$trigger_probe_count != 1} {
                set err_msg "Exactly one probe named 'trigger*' must be present, "
                append err_msg "but $trigger_probe_count were found" \n [all_probe_names_msg]
                error $err_msg
            } elseif {[get_property is_trigger $trigger_probe] != 1} {
                set probe_name_short [get_property name.short $trigger_probe]
                set err_msg "Probe '$probe_name_short' should have probeType "
                append err_msg {Trigger or DataAndTrigger} \n [all_probe_names_msg]
                error $err_msg
            } elseif {[get_property width $trigger_probe] != 1} {
                set probe_name_short [get_property name.short $trigger_probe]
                set err_msg "Probe '$probe_name_short' must have a width of 1 bit\n"
                append err_msg [all_probe_names_msg]
                error $err_msg
            } else {
                dict set ila_dict trigger_probe [get_property name $trigger_probe]
            }

            \# Get capture probe and verify it conforms with ILA framework
            set capture_probe [get_hw_probes -of_objects $hw_ila */capture*]
            set capture_probe_count [llength $capture_probe]
            if {$capture_probe_count != 1} {
                set err_msg {Exactly one probe named 'capture*' must be present, }
                append err_msg "but $capture_probe_count were found" \n [all_probe_names_msg]
                error $err_msg
            } elseif {[get_property is_trigger $capture_probe] != 1} {
                set probe_name_short [get_property name.short $capture_probe]
                set err_msg "Probe '$probe_name_short' should have probeType "
                append err_msg {Trigger or DataAndTrigger} \n [all_probe_names_msg]
                error $err_msg
            } elseif {[get_property width $capture_probe] != 1} {
                set probe_name_short [get_property name.short $capture_probe]
                set err_msg "Probe '$probe_name_short' must have a width of 1 bit\n"
                append err_msg [all_probe_names_msg]
                error $err_msg
            } else {
                dict set ila_dict capture_probe [get_property name $capture_probe]
            }

            \# Get all data probes and verify each conforms with ILA framework
            set all_probes [get_hw_probes -of_objects $hw_ila]
            if {[llength $all_probes] < 3} {
                set err_msg "ILA '$short_name' has no data probes, at least 1 "
                append err_msg {data probe is required} \n [all_probe_names_msg]
                error $err_msg
            }
            dict set ila_dict data_probes [list]
            foreach probe $all_probes {
                if {$probe eq $trigger_probe || $probe eq $capture_probe} {
                    continue
                } elseif {[get_property is_data $probe] != 1} {
                    set probe_name_short [get_property name.short $probe]
                    set err_msg "Probe '$probe_name_short' should have probeType "
                    append err_msg {Data or DataAndTrigger} \n [all_probe_names_msg]
                    error $err_msg
                } else {
                    dict update ila_dict data_probes probe_list {
                        lappend probe_list [get_property name $probe]
                    }
                }
            }
            lappend ila_dicts $ila_dict

            \# Print all ILA probes
            puts "ILA $short_name with probes:"
            set probe_name_short [get_property name.short $trigger_probe]
            puts "\t$probe_name_short"
            set probe_name_short [get_property name.short $capture_probe]
            puts "\t$probe_name_short"
            foreach probe_name [dict get $ila_dict data_probes] {
                set idx_start [expr {[string first / $probe_name] + 1}]
                set probe_name_short [string range $probe_name $idx_start end]
                puts "\t$probe_name_short"
            }
        }
        return $ila_dicts
    }
  |]

-- * Polling for test completion

{- | Waits (with a timeout) until a HITL test case is finished by probing
the probe_test_done probe. Returns whether the test case was successful.
-}
waitTestCaseEnd ::
  VivadoHandle -> String -> [(HwTarget, DeviceInfo)] -> FilePath -> IO ExitCode
waitTestCaseEnd v name testData probesFilePath = do
  startTime <- getTime Monotonic
  let calcTimeSpentMs = (`div` 1000000) . toNanoSecs . diffTimeSpec startTime <$> getTime Monotonic
  exitCodes <- forM testData $ \(hwT, _) -> do
    openHwTarget v hwT
    execCmd_ v "set_property" ["PROBES.FILE", embrace probesFilePath, "[current_hw_device]"]
    pollTestDone startTime testTimeoutMs v hwT

  -- Print summary of test case
  timeSpentMs <- calcTimeSpentMs
  putStrLn $
    "HITL test case'"
      <> name
      <> "' passed on "
      <> show (length (filter (== ExitSuccess) exitCodes))
      <> " out of "
      <> show (length exitCodes)
      <> " hardware targets in "
      <> show timeSpentMs
      <> "ms."
  pure (maximum exitCodes)
 where
  -- \| Timeout specifying how long we should wait for a test to finish before
  -- considering it a failed test.
  -- TODO: Allow the user to specify the timeout for a test.
  testTimeoutMs = 60000 :: Integer

pollTestDone :: TimeSpec -> Integer -> VivadoHandle -> HwTarget -> IO ExitCode
pollTestDone startTime testTimeoutMs v hwT = do
  refresh_hw_device v ["-quiet"]
  timeSpentMs <- calcTimeSpentMs
  done <- execCmd v "get_property" ["INPUT_VALUE", getProbeTestDoneTcl]
  success <- execCmd v "get_property" ["INPUT_VALUE", getProbeTestSuccessTcl]
  case (done, success, timeSpentMs >= testTimeoutMs) of
    ("1", "1", _) -> do
      pure ExitSuccess
    ("1", _, _) -> do
      putStrLn $ "HITL test case failure for hardware target " <> prettyShow hwT
      pure (ExitFailure 2)
    (_, _, True) -> do
      putStrLn $
        "HITL test case timeout (≥"
          <> show testTimeoutMs
          <> "ms) for hardware target "
          <> prettyShow hwT
      pure (ExitFailure 3)
    _ -> do
      threadDelay 1000 -- In μs
      pollTestDone startTime testTimeoutMs v hwT
 where
  calcTimeSpentMs = (`div` 1000000) . toNanoSecs . diffTimeSpec startTime <$> getTime Monotonic

mkUniqShortName :: MVar (Set String) -> String -> String -> IO String
mkUniqShortName refUsedNames shortName name =
  modifyMVar refUsedNames $ \usedNames -> do
    let
      nm0 = shortName
      nm1 = nm0 <> "_" <> name
      nm
        | Set.notMember nm0 usedNames = nm0
        | Set.notMember nm1 usedNames = nm1
        | otherwise =
            error [i|Failed to create unique shortname for #{name}, original shortname #{shortName}|]
    return (Set.insert nm usedNames, nm)
