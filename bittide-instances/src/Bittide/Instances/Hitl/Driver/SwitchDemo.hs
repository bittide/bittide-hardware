-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}
-- TODO: Remove use of partial functions
{-# OPTIONS_GHC -Wno-x-partial #-}

module Bittide.Instances.Hitl.Driver.SwitchDemo where

import Clash.Prelude

import Bittide.ClockControl.Config (CcConf, defCcConf, saveCcConfig)
import Bittide.ClockControl.Topology (Topology)
import Bittide.Hitl
import Bittide.Instances.Domains
import Bittide.Instances.Hitl.Setup (FpgaCount, fpgaSetup)
import Bittide.Instances.Hitl.SwitchDemo (memoryMapCc, memoryMapMu)
import Bittide.Instances.Hitl.Utils.Driver
import Bittide.Instances.Hitl.Utils.Program
import Bittide.Wishbone (TimeCmd (Capture))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently_, mapConcurrently_)
import Control.Concurrent.Async.Extra (zipWithConcurrently, zipWithConcurrently3_)
import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class
import Data.Char (ord)
import Data.Maybe (fromJust, fromMaybe)
import Data.String.Interpolate (i)
import GHC.Stack (HasCallStack)
import Gdb (Gdb)
import Numeric (showHex)
import Project.FilePath
import Project.Handle
import Protocols.MemoryMap (MemoryMap (..), MemoryMapTree (DeviceInstance, Interconnect))
import System.Exit
import System.FilePath
import System.IO
import System.Process (StdStream (CreatePipe, UseHandle))
import System.Timeout.Extra (tryWithTimeout, tryWithTimeoutOn)
import Text.Show.Pretty (ppShow)
import Vivado.Tcl (HwTarget)
import Vivado.VivadoM
import "bittide-extra" Control.Exception.Extra (brackets)

import qualified Bittide.Calculator as Calc
import qualified Bittide.Instances.Hitl.Utils.OpenOcd as Ocd
import qualified Bittide.Instances.Hitl.Utils.Picocom as Picocom
import qualified Clash.Sized.Vector as V
import qualified Data.List as L
import qualified Gdb

data OcdInitData = OcdInitData
  { muPort :: Int
  -- ^ Management unit GDB port
  , ccPort :: Int
  -- ^ Clock control GDB port
  , handles :: ProcessHandles
  -- ^ OpenOCD stdio handles
  , cleanup :: IO ()
  -- ^ Cleanup function
  }

data TestStatus = TestRunning | TestDone Bool | TestTimeout deriving (Eq)

type StartDelay = 5 -- seconds

type Padding = Calc.WindowCycles FpgaCount 3
type GppeConfig = Calc.DefaultGppeConfig FpgaCount Padding
type CyclesPerWrite = Calc.CalCyclesPerWrite GppeConfig
type GroupCycles = Calc.CalGroupCycles GppeConfig
type WindowCycles = Calc.CalWindowCycles GppeConfig
type ActiveCycles = Calc.CalActiveCycles GppeConfig
type MetacycleLength = Calc.CalMetacycleLength GppeConfig

gppeConfig :: GppeConfig
gppeConfig = Calc.defaultGppeCalcConfig

{- | Convert an integer to a zero-padded 32-bit hexadecimal string. The result
is prepended by \"0x\".

>>> showHex32 0xAB
"0x000000ab"
-}
showHex32 :: (Integral a) => a -> String
showHex32 a = "0x" <> padding <> hexStr
 where
  hexStr = showHex a ""
  padding = L.replicate (8 - L.length hexStr) '0'

{- | Get base addresses of a device from a memory map. Assumes that all devices
are located at a single interconnect, which is a bit grimy.
-}
baseAddresses :: (HasCallStack, Num a) => MemoryMap -> String -> [a]
baseAddresses memoryMap nm =
  case memoryMap of
    MemoryMap{tree = Interconnect _ devices} ->
      [fromIntegral addr | (addr, DeviceInstance _ d) <- devices, d == nm]
    _ -> error [i|Unexpected memory map structure: #{ppShow memoryMapMu}|]

{- | Like 'baseAddresses', but assume that there is only one device with the
given name in the memory map. If there are multiple devices with the same
name, an error is raised.
-}
baseAddress :: (HasCallStack, Num a, Show a) => MemoryMap -> String -> a
baseAddress memoryMap nm =
  case baseAddresses memoryMap nm of
    [addr] -> addr
    addrs -> error [i|Expected a single base address for #{nm}, got: #{show addrs}|]

-- | Like 'baseAddress', but specialized on the management unit memory map.
muBaseAddress :: (HasCallStack, Num a, Show a) => String -> a
muBaseAddress = baseAddress memoryMapMu

-- | Like 'baseAddress', but specialized on the clock control memory map.
ccBaseAddress :: (HasCallStack, Num a, Show a) => String -> a
ccBaseAddress = baseAddress memoryMapCc

{- | Addresses of the capture UGN devices in the management unit. As a sanity
check, it is expected that there are exactly 7 capture UGN devices present.
-}
muCaptureUgnAddresses :: (HasCallStack, Num a, Show a) => [a]
muCaptureUgnAddresses =
  if L.length ugnDevices == 7
    then ugnDevices
    else error $ "Expected 7 capture ugn devices, got: " <> ppShow ugnDevices
 where
  ugnDevices = baseAddresses memoryMapMu "CaptureUgn"

dumpCcSamples :: (HasCallStack) => FilePath -> CcConf Topology -> [Gdb] -> IO ()
dumpCcSamples hitlDir ccConf ccGdbs = do
  mapConcurrently_ Gdb.interrupt ccGdbs
  nSamples <- liftIO $ zipWithConcurrently go ccGdbs ccSamplesPaths
  putStrLn [i|Dumped /n/ clock control samples: #{nSamples}|]
  saveCcConfig hitlDir ccConf
  putStrLn [i|Wrote configs and samples to: #{hitlDir}|]
 where
  go :: (HasCallStack) => Gdb -> FilePath -> IO Word
  go gdb dumpPath = do
    nSamplesWritten <- Gdb.readLe @(Unsigned 32) gdb sampleMemoryBase

    let
      bytesPerSample = 13
      bytesPerWord = 4

      dumpStart = sampleMemoryBase + bytesPerWord
      dumpEnd = dumpStart + fromIntegral nSamplesWritten * bytesPerWord * bytesPerSample

    Gdb.dumpMemoryRegion gdb dumpPath dumpStart dumpEnd >> pure (numConvert nSamplesWritten)

  sampleMemoryBase = ccBaseAddress @Integer "SampleMemory"
  ccSamplesPaths = [[i|#{hitlDir}/cc-samples-#{n}.bin|] | n <- [(0 :: Int) .. 7]]

initOpenOcd :: FilePath -> (HwTarget, DeviceInfo) -> Int -> IO OcdInitData
initOpenOcd hitlDir (_, d) targetIndex = do
  putStrLn $ "Starting OpenOCD for target " <> d.deviceId

  let
    gdbPortMU = 3333 + targetIndex * 2
    gdbPortCC = gdbPortMU + 1
    tclPortMU = 6666 + targetIndex * 2
    tclPortCC = tclPortMU + 1
    telnetPortMU = 4444 + targetIndex * 2
    telnetPortCC = telnetPortMU + 1
    ocdStdout = hitlDir </> "openocd-" <> show targetIndex <> "-stdout.log"
    ocdStderr = hitlDir </> "openocd-" <> show targetIndex <> "-stderr.log"
  putStrLn $ "logging OpenOCD stdout to `" <> ocdStdout <> "`"
  putStrLn $ "logging OpenOCD stderr to `" <> ocdStderr <> "`"

  putStrLn "Starting OpenOCD..."
  (ocd, ocdPh, ocdClean0) <-
    Ocd.startOpenOcdWithEnvAndArgs
      ["-f", "sipeed.tcl", "-f", "vexriscv-2chain.tcl"]
      [ ("OPENOCD_STDOUT_LOG", ocdStdout)
      , ("OPENOCD_STDERR_LOG", ocdStderr)
      , ("USB_DEVICE", d.usbAdapterLocation)
      , ("DEV_A_GDB", show gdbPortMU)
      , ("DEV_B_GDB", show gdbPortCC)
      , ("DEV_A_TCL", show tclPortMU)
      , ("DEV_B_TCL", show tclPortCC)
      , ("DEV_A_TEL", show telnetPortMU)
      , ("DEV_B_TEL", show telnetPortCC)
      ]
  hSetBuffering ocd.stderrHandle LineBuffering
  tryWithTimeout "Waiting for OpenOCD to start" 15_000_000
    $ expectLine ocd.stderrHandle Ocd.waitForHalt

  let
    ocdProcName = "OpenOCD (" <> d.deviceId <> ")"
    ocdClean1 = ocdClean0 >> awaitProcessTermination ocdProcName ocdPh (Just 10_000_000)

  return $ OcdInitData gdbPortMU gdbPortCC ocd ocdClean1

initGdb ::
  FilePath ->
  String ->
  Gdb ->
  Int ->
  (HwTarget, DeviceInfo) ->
  IO ()
initGdb hitlDir binName gdb gdbPort (hwT, _d) = do
  Gdb.setLogging gdb
    $ hitlDir
    </> "gdb-" <> binName <> "-" <> show (getTargetIndex hwT) <> ".log"
  Gdb.setFile gdb $ firmwareBinariesDir "riscv32imc" Release </> binName
  Gdb.setTarget gdb gdbPort
  Gdb.setTimeout gdb Nothing
  Gdb.runCommand gdb "echo connected to target device"
  pure ()

initPicocom :: FilePath -> (HwTarget, DeviceInfo) -> Int -> IO (ProcessHandles, IO ())
initPicocom hitlDir (_hwTarget, deviceInfo) targetIndex = do
  devNullHandle <- openFile "/dev/null" WriteMode

  let
    devPath = deviceInfo.serial
    stdoutPath = hitlDir </> "picocom-" <> show targetIndex <> "-stdout.log"
    stderrPath = hitlDir </> "picocom-" <> show targetIndex <> "-stderr.log"

  -- Note that script at `devPath` already logs to `stdoutPath` and
  -- `stderrPath`. This is what we're after: debug logging. To prevent race
  -- conditions, we need to know when picocom is ready so we also shortly
  -- interested in stderr in this Haskell process.
  (pico, cleanup) <-
    Picocom.startWithLoggingAndEnv
      ( Picocom.StdStreams
          { Picocom.stdin = CreatePipe
          , Picocom.stdout = CreatePipe
          , Picocom.stderr = UseHandle devNullHandle
          }
      )
      devPath
      stdoutPath
      stderrPath
      []

  hSetBuffering pico.stdoutHandle LineBuffering

  tryWithTimeout "Waiting for \"Terminal ready\"" 10_000_000
    $ waitForLine pico.stdoutHandle "Terminal ready"

  pure (pico, cleanup)

ccGdbCheck :: Gdb -> VivadoM ExitCode
ccGdbCheck gdb = do
  let whoAmIBase = ccBaseAddress @Integer "WhoAmI"
  bytes <- Gdb.readBytes @4 gdb whoAmIBase
  let bytesAsInts = L.map fromIntegral $ V.toList $ bytes
  pure $ if bytesAsInts == L.map ord "swcc" then ExitSuccess else ExitFailure 1

muGdbCheck :: Gdb -> VivadoM ExitCode
muGdbCheck gdb = do
  let whoAmIBase = muBaseAddress @Integer "WhoAmI"
  bytes <- Gdb.readBytes @4 gdb whoAmIBase
  let bytesAsInts = L.map fromIntegral $ V.toList $ bytes
  pure $ if bytesAsInts == L.map ord "mgmt" then ExitSuccess else ExitFailure 1

foldExitCodes :: VivadoM (Int, ExitCode) -> ExitCode -> VivadoM (Int, ExitCode)
foldExitCodes prev code = do
  (count, acc) <- prev
  return
    $ if code == ExitSuccess
      then (count + 1, acc)
      else (count, code)

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

    muGetUgns :: (HwTarget, DeviceInfo) -> Gdb -> IO [(Unsigned 64, Unsigned 64)]
    muGetUgns (_, d) gdb = do
      let
        readUgnMmio :: Integer -> IO (Unsigned 64, Unsigned 64)
        readUgnMmio addr = Gdb.readLe gdb addr

      liftIO $ putStrLn $ "Getting UGNs for device " <> d.deviceId
      mapM readUgnMmio muCaptureUgnAddresses

    muReadPeBuffer :: (HasCallStack) => (HwTarget, DeviceInfo) -> Gdb -> IO ()
    muReadPeBuffer (_, d) gdb = do
      putStrLn $ "Reading PE buffer from device " <> d.deviceId
      let
        start = muBaseAddress @Integer "SwitchDemoPE"
        bufferSize :: Integer
        bufferSize = (snatToNum (SNat @FpgaCount)) * 3
      output <- Gdb.readCommandRaw gdb [i|x/#{bufferSize}xg #{showHex32 (start + 0x28)}|]
      putStrLn [i|PE buffer readout: #{output}|]

    muGetCurrentTime :: (HasCallStack) => (HwTarget, DeviceInfo) -> Gdb -> IO (Unsigned 64)
    muGetCurrentTime (_, d) gdb = do
      putStrLn $ "Getting current time from device " <> d.deviceId
      let timerBase = muBaseAddress @Integer "Timer"
      Gdb.writeLe gdb timerBase Capture
      Gdb.readLe gdb (timerBase + 0x8)

    muWriteCfg ::
      (HasCallStack) =>
      (HwTarget, DeviceInfo) ->
      Gdb ->
      Calc.CyclePeConfig (Unsigned 64) (Index 9) ->
      VivadoM ()
    muWriteCfg target@(_, d) gdb cfg = do
      let start = muBaseAddress "SwitchDemoPE"

      liftIO $ do
        muReadPeBuffer target gdb
        putStrLn $ "Writing config to device " <> d.deviceId
        Gdb.writeLe @(Unsigned 64) gdb (start + 0x00) cfg.startReadAt
        Gdb.writeLe @(Unsigned 64) gdb (start + 0x08) (numConvert cfg.readForN)
        Gdb.writeLe @(Unsigned 64) gdb (start + 0x10) cfg.startWriteAt
        Gdb.writeLe @(Unsigned 64) gdb (start + 0x18) (numConvert cfg.writeForN)

    finalCheck ::
      (HasCallStack) =>
      [Gdb] ->
      [Calc.CyclePeConfig (Unsigned 64) (Index 9)] ->
      VivadoM ExitCode
    finalCheck [] _ = fail "Should pass in two or more GDBs for the final check!"
    finalCheck (_ : []) _ = fail "Should pass in two or more GDBs for the final check!"
    finalCheck gdbs@(headGdb : _) configs = do
      let
        perDeviceCheck :: Gdb -> Int -> IO Bool
        perDeviceCheck myGdb num = do
          let
            headBaseAddr = muBaseAddress "SwitchDemoPE" + 0x28
            myBaseAddr = toInteger (headBaseAddr + 24 * (L.length gdbs - num - 1))
            dnaBaseAddr = muBaseAddress @Integer "Dna"
          myCounter <- Gdb.readLe @(Unsigned 64) headGdb myBaseAddr
          myDeviceDna2 <- Gdb.readLe @(Unsigned 32) myGdb dnaBaseAddr
          myDeviceDna1 <- Gdb.readLe @(Unsigned 32) myGdb (dnaBaseAddr + 0x4)
          myDeviceDna0 <- Gdb.readLe @(Unsigned 32) myGdb (dnaBaseAddr + 0x8)
          headDeviceDna2 <- Gdb.readLe @(Unsigned 32) headGdb (myBaseAddr + 0x08)
          headDeviceDna1 <- Gdb.readLe @(Unsigned 32) headGdb (myBaseAddr + 0x0C)
          headDeviceDna0 <- Gdb.readLe @(Unsigned 32) headGdb (myBaseAddr + 0x10)
          let
            myCfg = configs L.!! num
            expectedCounter = myCfg.startWriteAt
            counterEq = myCounter == expectedCounter
            dna0Eq = myDeviceDna0 == headDeviceDna0
            dna1Eq = myDeviceDna1 == headDeviceDna1
            dna2Eq = myDeviceDna2 == headDeviceDna2
          when (not counterEq)
            $ putStrLn
              [i|Counter #{num} did not match. Found #{myCounter}, expected #{expectedCounter}|]
          when (not dna0Eq)
            $ putStrLn
              [i|DNA0 #{num} did not match. Found #{headDeviceDna0}, expected #{myDeviceDna0}|]
          when (not dna1Eq)
            $ putStrLn
              [i|DNA1 #{num} did not match. Found #{headDeviceDna1}, expected #{myDeviceDna1}|]
          when (not dna2Eq)
            $ putStrLn
              [i|DNA2 #{num} did not match. Found #{headDeviceDna2}, expected #{myDeviceDna2}|]
          return $ counterEq && dna0Eq && dna1Eq && dna2Eq

      deviceChecks <- forM (L.zip gdbs [0 ..]) $ \(gdb, n) -> liftIO $ perDeviceCheck gdb n
      let result = if and deviceChecks then ExitSuccess else ExitFailure 2
      return result

  forM_ targets (assertProbe "probe_test_start")
  tryWithTimeout "Wait for handshakes successes from all boards" 30_000_000
    $ awaitHandshakes targets
  let openOcdStarts = liftIO <$> L.zipWith (initOpenOcd hitlDir) targets [0 ..]
  brackets openOcdStarts (liftIO . (.cleanup)) $ \initOcdsData -> do
    let
      muPorts = (.muPort) <$> initOcdsData
      ccPorts = (.ccPort) <$> initOcdsData
    Gdb.withGdbs (L.length targets) $ \ccGdbs -> do
      liftIO $ zipWithConcurrently3_ (initGdb hitlDir "clock-control") ccGdbs ccPorts targets
      liftIO $ putStrLn "Checking for MMIO access to SwCC CPUs over GDB..."
      gdbExitCodes0 <- mapM ccGdbCheck ccGdbs
      (gdbCount0, gdbExitCode0) <-
        L.foldl foldExitCodes (pure (0, ExitSuccess)) gdbExitCodes0
      liftIO
        $ putStrLn
          [i|CC GDB testing passed on #{gdbCount0} of #{L.length targets} targets|]
      liftIO $ mapConcurrently_ ((errorToException =<<) . Gdb.loadBinary) ccGdbs

      Gdb.withGdbs (L.length targets) $ \muGdbs -> do
        liftIO $ zipWithConcurrently3_ (initGdb hitlDir "management-unit") muGdbs muPorts targets
        liftIO $ putStrLn "Checking for MMIO access to MU CPUs over GDB..."
        gdbExitCodes1 <- mapM muGdbCheck muGdbs
        (gdbCount1, gdbExitCode1) <-
          L.foldl foldExitCodes (pure (0, ExitSuccess)) gdbExitCodes1
        liftIO
          $ putStrLn
            [i|MU GDB testing passed on #{gdbCount1} of #{L.length targets} targets|]
        liftIO $ mapConcurrently_ ((errorToException =<<) . Gdb.loadBinary) muGdbs

        let picocomStarts = liftIO <$> L.zipWith (initPicocom hitlDir) targets [0 ..]
        brackets picocomStarts (liftIO . snd) $ \(L.map fst -> picocoms) -> do
          let goDumpCcSamples = dumpCcSamples hitlDir (defCcConf (natToNum @FpgaCount)) ccGdbs
          liftIO $ mapConcurrently_ Gdb.continue ccGdbs
          liftIO $ mapConcurrently_ Gdb.continue muGdbs
          liftIO
            $ tryWithTimeoutOn "Waiting for stable links" 60_000_000 goDumpCcSamples
            $ forConcurrently_ picocoms
            $ \pico ->
              waitForLine pico.stdoutHandle "[CC] All links stable"

          liftIO $ putStrLn "Getting UGNs for all targets"
          liftIO $ mapConcurrently_ Gdb.interrupt muGdbs
          ugnPairsTable <- liftIO $ zipWithConcurrently muGetUgns targets muGdbs
          let
            ugnPairsTableV = fromJust . V.fromList $ fromJust . V.fromList <$> ugnPairsTable
          liftIO $ do
            putStrLn "Calculating IGNs for all targets"
            Calc.printAllIgns ugnPairsTableV fpgaSetup
            mapM_ print ugnPairsTableV
          currentTime <- liftIO $ muGetCurrentTime (L.head targets) (L.head muGdbs)
          let
            startOffset = currentTime + natToNum @(PeriodToCycles GthTx (Seconds StartDelay))
            metaChainConfig ::
              Vec FpgaCount (Calc.DefaultGppeMetaPeConfig (Unsigned 64) FpgaCount 3 Padding)
            metaChainConfig =
              Calc.fullChainConfiguration gppeConfig fpgaSetup ugnPairsTableV startOffset
            chainConfig :: Vec FpgaCount (Calc.CyclePeConfig (Unsigned 64) (Index (FpgaCount + 1)))
            chainConfig =
              Calc.metaPeConfigToCyclePeConfig (natToNum @MetacycleLength)
                <$> metaChainConfig
          liftIO $ do
            putStrLn [i|Starting clock cycle: #{startOffset}|]
            putStrLn [i|Cycles per write: #{natToNum @CyclesPerWrite :: Integer}|]
            putStrLn [i|Cycles per group: #{natToNum @GroupCycles :: Integer}|]
            putStrLn [i|Cycles per window: #{natToNum @WindowCycles :: Integer}|]
            putStrLn [i|Cycles per active period: #{natToNum @ActiveCycles :: Integer}|]
            putStrLn [i|Cycles of padding: #{natToNum @Padding :: Integer}|]
            putStrLn [i|Cycles per metacycle: #{natToNum @MetacycleLength :: Integer}|]
            putStrLn "Calculated the following configs for the switch processing elements:"
            forM_ metaChainConfig print
            forM_ chainConfig print
          _ <- sequenceA $ L.zipWith3 muWriteCfg targets muGdbs (toList chainConfig)
          liftIO $ do
            let delayMicros = natToNum @StartDelay * 1_250_000
            threadDelay delayMicros
            putStrLn [i|Slept for: #{delayMicros}μs|]
            newCurrentTime <- muGetCurrentTime (L.head targets) (L.head muGdbs)
            putStrLn [i|Clock is now: #{newCurrentTime}|]

          _ <- liftIO $ sequenceA $ L.zipWith muReadPeBuffer targets muGdbs

          bufferExit <- finalCheck muGdbs (toList chainConfig)

          liftIO goDumpCcSamples

          pure
            $ fromMaybe ExitSuccess
            $ L.find (/= ExitSuccess) [gdbExitCode0, gdbExitCode1, bufferExit]
