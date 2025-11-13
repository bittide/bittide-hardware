-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}

module Bittide.Instances.Hitl.Driver.SoftUgnDemo where

import Clash.Prelude

import Bittide.ClockControl.Config (defCcConf)
import Bittide.Hitl
import Bittide.Instances.Hitl.Driver.SwitchDemo (
  dumpCcSamples,
  foldExitCodes,
  getPathAddress,
  initGdb,
  initPicocom,
  showHex32,
 )
import Bittide.Instances.Hitl.Dut.SoftUgnDemo (
  ccWhoAmID,
  gppeWhoAmID,
  memoryMapCc,
  memoryMapGppe,
  memoryMapMu,
  muWhoAmID,
 )
import Bittide.Instances.Hitl.Setup (FpgaCount)
import Bittide.Instances.Hitl.Utils.Driver
import Bittide.Instances.Hitl.Utils.Program
import Control.Concurrent.Async (forConcurrently_, mapConcurrently_)
import Control.Concurrent.Async.Extra (zipWithConcurrently3_)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class
import Control.Monad.Reader (ask)
import Data.Char (chr, isAscii, isPrint)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Vector.Internal.Check (HasCallStack)
import Gdb (Gdb)
import Project.FilePath
import Project.Handle
import Protocols.MemoryMap (MemoryMap)
import System.Exit
import System.FilePath
import System.IO
import Vivado (VivadoHandle)
import Vivado.Tcl (
  HwIla,
  HwTarget,
  current_hw_ila,
  execCmd,
  execCmd_,
  get_hw_ilas,
  openHwTarget,
  refresh_hw_device,
  run_hw_ila,
 )
import Vivado.VivadoM (VivadoM)
import "bittide-extra" Control.Exception.Extra (brackets)
import "extra" Data.List.Extra ((!?))

import qualified Bittide.Calculator as Calc
import qualified Bittide.Instances.Hitl.Utils.OpenOcd as Ocd
import qualified Clash.Sized.Vector as V
import qualified Data.List as L
import qualified Gdb
import qualified System.Timeout.Extra as T
import qualified "extra" Data.List.Extra as L

data OcdInitData = OcdInitData
  { muPort :: Int
  -- ^ Management unit GDB port
  , ccPort :: Int
  -- ^ Clock control GDB port
  , gppePort :: Int
  -- ^ GPPE GDB port
  , handles :: ProcessHandles
  -- ^ OpenOCD stdio handles
  , cleanup :: IO ()
  -- ^ Cleanup function
  }

data TestStatus = TestRunning | TestDone Bool | TestTimeout deriving (Eq)

type StartDelay = 10 -- seconds

type Padding = Calc.WindowCycles FpgaCount 3
type GppeConfig = Calc.DefaultGppeConfig FpgaCount Padding
type CyclesPerWrite = Calc.CalCyclesPerWrite GppeConfig
type GroupCycles = Calc.CalGroupCycles GppeConfig
type WindowCycles = Calc.CalWindowCycles GppeConfig
type ActiveCycles = Calc.CalActiveCycles GppeConfig
type MetacycleLength = Calc.CalMetacycleLength GppeConfig

gppeConfig :: GppeConfig
gppeConfig = Calc.defaultGppeCalcConfig

initOpenOcd :: FilePath -> (HwTarget, DeviceInfo) -> Int -> IO OcdInitData
initOpenOcd hitlDir (_, d) targetIndex = do
  putStrLn $ "Starting OpenOCD for target " <> d.deviceId

  let
    gdbPortMU = 3333 + targetIndex * 3
    gdbPortCC = gdbPortMU + 1
    gdbPortGPPE = gdbPortMU + 2
    tclPort = 6666 + targetIndex
    telnetPort = 4444 + targetIndex
    ocdStdout = hitlDir </> "openocd-" <> show targetIndex <> "-stdout.log"
    ocdStderr = hitlDir </> "openocd-" <> show targetIndex <> "-stderr.log"
  putStrLn $ "logging OpenOCD stdout to `" <> ocdStdout <> "`"
  putStrLn $ "logging OpenOCD stderr to `" <> ocdStderr <> "`"

  putStrLn "Starting OpenOCD..."
  (ocd, ocdPh, ocdClean0) <-
    Ocd.startOpenOcdWithEnvAndArgs
      ["-f", "sipeed.tcl", "-f", "vexriscv-3chain.tcl"]
      [ ("OPENOCD_STDOUT_LOG", ocdStdout)
      , ("OPENOCD_STDERR_LOG", ocdStderr)
      , ("USB_DEVICE", d.usbAdapterLocation)
      , ("DEV_A_GDB", show gdbPortGPPE)
      , ("DEV_B_GDB", show gdbPortCC)
      , ("DEV_C_GDB", show gdbPortMU)
      , ("TCL_PORT", show tclPort)
      , ("TEL_PORT", show telnetPort)
      ]
  hSetBuffering ocd.stderrHandle LineBuffering
  T.tryWithTimeout T.PrintActionTime "Waiting for OpenOCD to start" 15_000_000
    $ expectLine ocd.stderrHandle Ocd.waitForHalt

  let
    ocdProcName = "OpenOCD (" <> d.deviceId <> ")"
    ocdClean1 = ocdClean0 >> awaitProcessTermination ocdProcName ocdPh (Just 10_000_000)

  return $ OcdInitData gdbPortMU gdbPortCC gdbPortGPPE ocd ocdClean1

lookForWhoAmI :: MemoryMap -> Integer
lookForWhoAmI mm = getPathAddress mm ["0", "WhoAmI", "identifier"]

readWhoAmID :: Integer -> Gdb -> IO (Either [Int] String)
readWhoAmID addr gdb = do
  bytes <- fmap (fmap fromIntegral) $ Gdb.readBytes @4 gdb addr
  let chars = chr <$> bytes
  return
    $ if all (\c -> isAscii c && isPrint c) chars
      then Right $ V.toList chars
      else Left $ V.toList bytes

-- Copied from `Clash.Shake.Vivado`. we dont want to depend on that package.
getIlaShortName :: VivadoHandle -> HwIla -> IO String
getIlaShortName v ila = do
  -- First, set this ILA as the current one in Tcl
  _ <- current_hw_ila v [show ila]
  -- Now use [current_hw_ila] to reference the Tcl object
  ilaCellName <- execCmd v "get_property" ["CELL_NAME", "[current_hw_ila]"]
  pure
    $ fromMaybe
      (error $ "Determining short name failed for ILA with CELL_NAME " <> ilaCellName)
      (L.reverse (L.split (== '/') ilaCellName) !? 1)

{- | Given an expected @whoAmID@ and a GDB process connected to a target CPU, check
whether the target CPU reports the expected @whoAmID@.
-}
gdbCheck :: BitVector 32 -> Integer -> [(String, Integer)] -> Gdb -> VivadoM ExitCode
gdbCheck expectedBE expectedAddr altAddrs gdb = do
  let expectedIdent = whoAmIdToString expectedBE
  maybeId <- liftIO $ readWhoAmID expectedAddr gdb
  case maybeId of
    Right ident -> do
      liftIO $ putStrLn [i|Read whoAmID: '#{ident}', expected '#{expectedIdent}'|]
      if ident == expectedIdent
        then return ExitSuccess
        else do
          liftIO $ forM_ altAddrs altAddrCmp
          return $ ExitFailure 1
    Left malformed -> do
      liftIO $ putStrLn [i|Read malformed ID: #{malformed}. Attempting alternatives...|]
      liftIO $ forM_ altAddrs altAddrCmp
      return $ ExitFailure 1
 where
  whoAmIdToString :: BitVector 32 -> String
  whoAmIdToString whoAmID = L.map (chr . fromIntegral) $ V.toList expectedLE
   where
    expectedLE = reverse $ bitCoerce @_ @(Vec 4 (BitVector 8)) whoAmID

  altAddrCmp :: (String, Integer) -> IO ()
  altAddrCmp (name, addr) = do
    maybeId <- readWhoAmID addr gdb
    case maybeId of
      Right ident -> putStrLn [i|Address for #{name} gives ID '#{ident}'|]
      Left ints -> putStrLn [i|Address for #{name} gives malformed ID. Raw: #{ints}|]

{- | Arm the bittidePeIla ILA specifically. This function runs in VivadoM and configures
and arms the ILA with trigger and capture probes set to detect activity.
The function should be called after calendar initialization is complete.
-}
armBittidePeIla :: [(HwTarget, DeviceInfo)] -> VivadoM ()
armBittidePeIla targets = do
  v <- ask -- Get the VivadoHandle from the ReaderT context
  forM_ targets $ \(hwTarget, deviceInfo) -> do
    liftIO $ do
      putStrLn $ "[DEBUG] Starting ILA arming for device " <> deviceInfo.deviceId <> "..."
      putStrLn $ "[DEBUG] Hardware target: " <> show hwTarget

      -- Open the hardware target to access its ILAs
      putStrLn "[DEBUG] Opening hardware target..."
      openHwTarget v hwTarget
      putStrLn "[DEBUG] Refreshing hardware device..."
      refresh_hw_device v []

      -- Get all ILAs for this target
      putStrLn "[DEBUG] Getting list of ILAs..."
      ilas <- get_hw_ilas v []
      putStrLn $ "[DEBUG] Found " <> show (L.length ilas) <> " ILA(s)"

      -- Debug: print all ILAs
      forM_ (L.zip [(1 :: Int) ..] ilas) $ \(idx, ila) -> do
        putStrLn $ "[DEBUG] ILA #" <> show idx <> ": " <> show ila

      -- Find and arm the bittidePeIla specifically
      forM_ ilas $ \ila -> do
        putStrLn $ "[DEBUG] Processing ILA: " <> show ila
        putStrLn $ "[DEBUG] Attempting to get ILA short name..."
        ilaName <- getIlaShortName v ila
        putStrLn $ "[DEBUG] ILA short name: " <> ilaName

        -- Check if this is the bittidePeIla
        when ("bittidePeIla" `L.isInfixOf` ilaName) $ do
          putStrLn $ "[DEBUG] Found matching bittidePeIla: " <> ilaName

          -- Select this ILA as current
          putStrLn "[DEBUG] Setting current ILA..."
          _ <- current_hw_ila v [show ila]

          -- Configure trigger probe (trigger on activity = 1)
          putStrLn "[DEBUG] Configuring trigger probe..."
          let triggerProbe = "[get_hw_probes -of_objects [current_hw_ila] */trigger*]"
          execCmd_ v "set_property" ["trigger_compare_value", "eq1'b1", triggerProbe]

          -- Enable capture control and set capture probe
          putStrLn "[DEBUG] Configuring capture mode..."
          execCmd_ v "set_property" ["control.capture_mode", "BASIC", "[current_hw_ila]"]
          putStrLn "[DEBUG] Configuring capture probe..."
          let captureProbe = "[get_hw_probes -of_objects [current_hw_ila] */capture*]"
          execCmd_ v "set_property" ["capture_compare_value", "eq1'b1", captureProbe]

          -- Set trigger position (0 = trigger at start of capture window)
          putStrLn "[DEBUG] Setting trigger position..."
          execCmd_ v "set_property" ["control.trigger_position", "0", "[current_hw_ila]"]

          -- ARM THE ILA
          putStrLn "[DEBUG] Arming ILA..."
          run_hw_ila v ["[current_hw_ila]"]
          putStrLn $ "[DEBUG] Successfully armed bittidePeIla for " <> deviceInfo.deviceId

driver ::
  (HasCallStack) =>
  String ->
  [(HwTarget, DeviceInfo)] ->
  VivadoM ExitCode
driver testName targets = do
  liftIO
    . putStrLn
    $ "Running driver function for targets "
    <> show ((\(_, info) -> info.deviceId) <$> targets)

  projectDir <- liftIO $ findParentContaining "cabal.project"

  let
    hitlDir = projectDir </> "_build/hitl" </> testName

  forM_ targets (assertProbe "probe_test_start")
  T.tryWithTimeout
    T.PrintActionTime
    "Wait for handshakes successes from all boards"
    30_000_000
    $ awaitHandshakes targets
  let openOcdStarts = liftIO <$> L.zipWith (initOpenOcd hitlDir) targets [0 ..]
  brackets openOcdStarts (liftIO . (.cleanup)) $ \initOcdsData -> do
    let
      muPorts = (.muPort) <$> initOcdsData
      ccPorts = (.ccPort) <$> initOcdsData
      gppePorts = (.gppePort) <$> initOcdsData
      ccWAIAddr = lookForWhoAmI memoryMapCc
      muWAIAddr = lookForWhoAmI memoryMapMu
      peWAIAddr = lookForWhoAmI memoryMapGppe
      ccAlt = ("CC", ccWAIAddr)
      muAlt = ("MU", muWAIAddr)
      peAlt = ("PE", peWAIAddr)

    Gdb.withGdbs (L.length targets) $ \ccGdbs -> do
      liftIO $ zipWithConcurrently3_ (initGdb hitlDir "clock-control") ccGdbs ccPorts targets
      liftIO $ putStrLn "Checking for MMIO access to SWCC CPUs over GDB..."
      liftIO $ putStrLn [i|Using address #{showHex32 ccWAIAddr}|]
      gdbExitCodes0 <- mapM (gdbCheck ccWhoAmID ccWAIAddr [muAlt, peAlt]) ccGdbs
      (gdbCount0, gdbExitCode0) <-
        L.foldl foldExitCodes (pure (0, ExitSuccess)) gdbExitCodes0
      liftIO
        $ putStrLn
          [i|CC GDB testing passed on #{gdbCount0} of #{L.length targets} targets|]
      liftIO $ mapConcurrently_ ((errorToException =<<) . Gdb.loadBinary) ccGdbs

      Gdb.withGdbs (L.length targets) $ \muGdbs -> do
        liftIO $ zipWithConcurrently3_ (initGdb hitlDir "soft-ugn-mu") muGdbs muPorts targets
        liftIO $ putStrLn "Checking for MMIO access to MU CPUs over GDB..."
        liftIO $ putStrLn [i|Using address #{showHex32 muWAIAddr}|]
        gdbExitCodes1 <- mapM (gdbCheck muWhoAmID muWAIAddr [ccAlt, peAlt]) muGdbs
        (gdbCount1, gdbExitCode1) <-
          L.foldl foldExitCodes (pure (0, ExitSuccess)) gdbExitCodes1
        liftIO
          $ putStrLn
            [i|MU GDB testing passed on #{gdbCount1} of #{L.length targets} targets|]
        liftIO $ mapConcurrently_ ((errorToException =<<) . Gdb.loadBinary) muGdbs

        Gdb.withGdbs (L.length targets) $ \gppeGdbs -> do
          liftIO
            $ zipWithConcurrently3_ (initGdb hitlDir "soft-ugn-gppe") gppeGdbs gppePorts targets
          liftIO $ putStrLn "Checking for MMIO access to GPPE CPUs over GDB..."
          liftIO $ putStrLn [i|Using address #{showHex32 peWAIAddr}|]
          gdbExitCodes2 <- mapM (gdbCheck gppeWhoAmID peWAIAddr [ccAlt, muAlt]) gppeGdbs
          (gdbCount2, gdbExitCode2) <-
            L.foldl foldExitCodes (pure (0, ExitSuccess)) gdbExitCodes2
          liftIO
            $ putStrLn
              [i|GPPE GDB testing passed on #{gdbCount2} of #{L.length targets} targets|]
          liftIO $ mapConcurrently_ ((errorToException =<<) . Gdb.loadBinary) gppeGdbs

          let picocomStarts = liftIO <$> L.zipWith (initPicocom hitlDir) targets [0 ..]
          brackets picocomStarts (liftIO . snd) $ \(L.map fst -> picocoms) -> do
            let goDumpCcSamples = dumpCcSamples hitlDir (defCcConf (natToNum @FpgaCount)) ccGdbs
            liftIO $ mapConcurrently_ Gdb.continue ccGdbs
            liftIO
              $ T.tryWithTimeoutOn T.PrintActionTime "Waiting for stable links" 60_000_000 goDumpCcSamples
              $ forConcurrently_ picocoms
              $ \pico ->
                waitForLine pico.stdoutHandle "[CC] All links stable"

            liftIO $ mapConcurrently_ Gdb.continue muGdbs
            liftIO
              $ T.tryWithTimeoutOn
                T.PrintActionTime
                "Wait for elastic buffers to be centered"
                60_000_000
                goDumpCcSamples
              $ forConcurrently_ picocoms
              $ \pico ->
                waitForLine pico.stdoutHandle "[MU] All elastic buffers centered"

            liftIO
              $ T.tryWithTimeoutOn
                T.PrintActionTime
                "Waiting for captured UGNs"
                (3 * 60_000_000)
                goDumpCcSamples
              $ forConcurrently_ picocoms
              $ \pico ->
                waitForLine pico.stdoutHandle "[MU] All UGNs captured"

            liftIO
              $ T.tryWithTimeoutOn
                T.PrintActionTime
                "Waiting for calendar initialization"
                (3 * 60_000_000)
                goDumpCcSamples
              $ forConcurrently_ picocoms
              $ \pico ->
                waitForLine pico.stdoutHandle "[MU] All calendars initialized"

            -- Arm the bittidePeIla ILA after calendars are initialized
            armBittidePeIla targets

            -- From here the actual test should be done, but for now it's just going to be
            -- waiting for the devices to print out over UART.

            liftIO $ mapConcurrently_ Gdb.continue gppeGdbs
            liftIO
              $ T.tryWithTimeoutOn
                T.PrintActionTime
                "Waiting for UGN discovery protocol to complete"
                (60_000_000)
                goDumpCcSamples
              $ forConcurrently_ picocoms
              $ \pico ->
                waitForLine pico.stdoutHandle "[PE] UGN discovery protocol complete!"

            liftIO goDumpCcSamples

            pure
              $ fromMaybe ExitSuccess
              $ L.find (/= ExitSuccess) [gdbExitCode0, gdbExitCode1, gdbExitCode2]
