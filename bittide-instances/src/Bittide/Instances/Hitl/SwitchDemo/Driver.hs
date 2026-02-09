-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}
-- TODO: Remove use of partial functions
{-# OPTIONS_GHC -Wno-x-partial #-}

module Bittide.Instances.Hitl.SwitchDemo.Driver where

import Clash.Prelude

import Bittide.ClockControl.Config (CcConf, defCcConf, saveCcConfig)
import Bittide.ClockControl.Topology (Topology)
import Bittide.Hitl
import Bittide.Instances.Domains (GthTx)
import Bittide.Instances.Hitl.Setup (FpgaCount, LinkCount, fpgaSetup)
import Bittide.Instances.Hitl.Utils.Driver
import Bittide.Instances.Hitl.Utils.MemoryMap (getPathAddress)
import Bittide.Instances.Hitl.Utils.Program
import Bittide.Wishbone (TimeCmd (Capture))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently_, mapConcurrently_)
import Control.Concurrent.Async.Extra (zipWithConcurrently, zipWithConcurrently3_)
import Control.Monad (forM, forM_, unless)
import Control.Monad.IO.Class
import Data.Maybe (fromJust)
import Data.String.Interpolate (i)
import GHC.Stack (HasCallStack)
import Gdb (Gdb)
import Numeric (showHex)
import Project.FilePath
import Project.Handle
import System.Exit
import System.FilePath
import System.IO
import System.Process (StdStream (CreatePipe, UseHandle))
import Vivado.Tcl (HwTarget)
import Vivado.VivadoM
import "bittide-extra" Control.Exception.Extra (brackets)

import qualified Bittide.Calculator as Calc
import qualified Bittide.Instances.Hitl.SwitchDemo.MemoryMaps as MemoryMaps
import qualified Bittide.Instances.Hitl.Utils.OpenOcd as Ocd
import qualified Bittide.Instances.Hitl.Utils.Picocom as Picocom
import qualified Clash.Sized.Vector as V
import qualified Data.List as L
import qualified Gdb
import qualified System.Timeout.Extra as T

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

muSwitchDemoPeBuffer :: (HasCallStack, Num a, Show a) => a
muSwitchDemoPeBuffer = getPathAddress MemoryMaps.mu ["0", "SwitchDemoPE", "buffer"]

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

  sampleMemoryBase = getPathAddress @Integer MemoryMaps.cc ["0", "SampleMemory", "data"]
  ccSamplesPaths = [[i|#{hitlDir}/cc-samples-#{n}.bin|] | n <- [(0 :: Int) .. 7]]

initGdb ::
  FilePath ->
  String ->
  Gdb ->
  Ocd.TapInfo ->
  (HwTarget, DeviceInfo) ->
  IO ()
initGdb hitlDir binName gdb tapInfo (hwT, _d) = do
  Gdb.setLogging gdb
    $ hitlDir
    </> "gdb-" <> binName <> "-" <> show (getTargetIndex hwT) <> ".log"
  Gdb.setFile gdb $ firmwareBinariesDir "riscv32imc" Release </> binName
  Gdb.setTarget gdb tapInfo.gdbPort
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
    Picocom.startWithLogging
      ( Picocom.StdStreams
          { Picocom.stdin = CreatePipe
          , Picocom.stdout = CreatePipe
          , Picocom.stderr = UseHandle devNullHandle
          }
      )
      devPath
      Picocom.parameters
        { Picocom.stdOut = stdoutPath
        , Picocom.stdErr = stderrPath
        }

  hSetBuffering pico.stdoutHandle LineBuffering

  T.tryWithTimeout T.PrintActionTime "Waiting for \"Terminal ready\"" 10_000_000
    $ waitForLine pico.stdoutHandle "Terminal ready"

  pure (pico, cleanup)

{- | Parse the tap info from OpenOCD log produced during startup. This function
expects to find multiple JTAG IDs and exactly one GDB port. This is typically
paired with 'vexriscv_boot_init.tcl' which only creates a target for the
last TAP (the boot CPU).
-}
parseBootTapInfo :: (HasCallStack) => Ocd.OcdInitData -> Ocd.TapInfo
parseBootTapInfo initOcd =
  case (L.sortOn snd <$> Ocd.parseJtagIds initOcd.log, Ocd.parseGdbPorts initOcd.log) of
    (Left err, _) -> error $ "Failed to parse JTAG IDs from OpenOCD log: " <> err
    (_, Left err) -> error $ "Failed to parse GDB ports from OpenOCD log: " <> err
    (Right [], Right _) -> error "No JTAG IDs found in OpenOCD log!"
    (Right _, Right (_ : _ : _)) -> error "Multiple GDB ports found in OpenOCD log!"
    (Right _, Right []) -> error "No GDB ports found in OpenOCD log!"
    (Right ((jtagTapId, jtagId) : _), Right [(gdbTapId, gdbPort)])
      | jtagTapId /= gdbTapId ->
          -- This can happen for a various number of reasons. Maybe check whether
          -- all tap ids are present, that there is only one GDB server, and that
          -- that GDB server is tied to a TAP ID that's equal to the TAP ID of the
          -- tap associated with the lowest JTAG IDCODE.
          error
            [i|Expected first JTAG TAP ID, ordered by associated IDCODE, to be equal to the JTAG TAP ID of the only GDB port found, but: #{jtagTapId} /= #{gdbTapId}|]
      | otherwise -> Ocd.TapInfo{tapId = jtagTapId, jtagId, gdbPort}

{- | Parse the tap info from OpenOCD log produced during startup. This function
expects to find all taps given to it. For each tap, it expects to find exactly one
GDB port.
-}
parseTapInfo :: (HasCallStack) => [Ocd.JtagId] -> Ocd.OcdInitData -> [Ocd.TapInfo]
parseTapInfo expectedJtagIds initOcd =
  case Ocd.parseJtagIdsAndGdbPorts initOcd.log of
    Left err -> error $ "Failed to parse TAP info from OpenOCD log: " <> err
    Right tapInfos
      | ((.jtagId) <$> tapInfos) /= expectedJtagIds ->
          error [i|Parsed JTAG IDs do not match expected IDs!|]
      | otherwise -> tapInfos

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
        captureUgnPrefixed :: Int -> String -> [String]
        captureUgnPrefixed linkNr reg = ["0", "CaptureUgn" <> show linkNr, reg]

        readUgnMmio :: Int -> IO (Unsigned 64, Unsigned 64, Signed 32)
        readUgnMmio linkNr = do
          let getUgnRegister = getPathAddress MemoryMaps.mu . captureUgnPrefixed linkNr
          localCounter <- Gdb.readLe gdb (getUgnRegister "local_counter")
          remoteCounter <- Gdb.readLe gdb (getUgnRegister "remote_counter")
          delta <- Gdb.readLe gdb (getUgnRegister "elastic_buffer_delta")
          pure (localCounter, remoteCounter, delta)

        -- Adjust the local counter for the frames added/removed from the elastic
        -- buffer after capturing the UGN. Leaves the remote counter untouched.
        adjustLocalCounter :: (Unsigned 64, Unsigned 64, Signed 32) -> (Unsigned 64, Unsigned 64)
        adjustLocalCounter (localCounter, remoteCounter, delta) =
          (addSigned localCounter delta, remoteCounter)
         where
          addSigned :: Unsigned 64 -> Signed 32 -> Unsigned 64
          addSigned u s = checkedFromIntegral (toInteger u + toInteger s)

      liftIO $ putStrLn $ "Getting UGNs for device " <> d.deviceId
      ugnTriples <- mapM readUgnMmio [0 .. natToNum @(LinkCount - 1)]
      liftIO $ forM_ ugnTriples $ \triple -> putStrLn $ "Raw UGN triple: " <> show triple
      pure $ adjustLocalCounter <$> ugnTriples

    muReadPeBuffer :: (HasCallStack) => (HwTarget, DeviceInfo) -> Gdb -> IO ()
    muReadPeBuffer (_, d) gdb = do
      putStrLn $ "Reading PE buffer from device " <> d.deviceId
      let
        bufferSize :: Integer
        bufferSize = (snatToNum (SNat @FpgaCount)) * 3
      output <-
        Gdb.readCommandRaw gdb [i|x/#{bufferSize}xg #{showHex32 @Integer muSwitchDemoPeBuffer}|]
      putStrLn [i|PE buffer readout:\n#{output}|]

    muGetCurrentTime :: (HasCallStack) => (HwTarget, DeviceInfo) -> Gdb -> IO (Unsigned 64)
    muGetCurrentTime (_, d) gdb = do
      putStrLn $ "Getting current time from device " <> d.deviceId
      Gdb.writeLe gdb (getPathAddress @Integer MemoryMaps.mu ["0", "Timer", "command"]) Capture
      Gdb.readLe gdb (getPathAddress @Integer MemoryMaps.mu ["0", "Timer", "scratchpad"])

    -- \| Read the elastic buffer under and overflow flags and verify they are clear
    checkElasticBufferFlags :: (HasCallStack) => (HwTarget, DeviceInfo) -> Gdb -> IO Bool
    checkElasticBufferFlags (_, d) gdb = do
      let
        ebPrefixed :: Int -> String -> [String]
        ebPrefixed linkNr reg = ["0", "ElasticBuffer" <> show linkNr, reg]

        readEbFlag :: Int -> IO (Bool, Bool)
        readEbFlag linkNr = do
          let getEbRegister = getPathAddress MemoryMaps.mu . ebPrefixed linkNr
          underflow <- Gdb.readLe gdb (getEbRegister "underflow")
          overflow <- Gdb.readLe gdb (getEbRegister "overflow")
          pure (underflow, overflow)

      flags <- mapM readEbFlag [0 .. natToNum @(LinkCount - 1)]
      let allClear = all (== (False, False)) flags
      liftIO $ unless allClear $ do
        let deviceId = d.deviceId
        -- Don't error here so all devices are checked
        putStrLn
          [i|[ERROR] Some elastic buffer under/overflowed on device #{deviceId} (under,over): #{flags}|]
      pure allClear

    muWriteCfg ::
      (HasCallStack) =>
      (HwTarget, DeviceInfo) ->
      Gdb ->
      Calc.CyclePeConfig (Unsigned 64) (Index 9) ->
      VivadoM ()
    muWriteCfg target@(_, d) gdb cfg = do
      let
        sdpePrefixed :: String -> [String]
        sdpePrefixed reg = ["0", "SwitchDemoPE", reg]

      liftIO $ do
        muReadPeBuffer target gdb
        putStrLn $ "Writing config to device " <> d.deviceId
        Gdb.writeLe @(Unsigned 64)
          gdb
          (getPathAddress MemoryMaps.mu (sdpePrefixed "read_start"))
          cfg.startReadAt
        Gdb.writeLe @(Unsigned 64)
          gdb
          (getPathAddress MemoryMaps.mu (sdpePrefixed "read_cycles"))
          (numConvert cfg.readForN)
        Gdb.writeLe @(Unsigned 64)
          gdb
          (getPathAddress MemoryMaps.mu (sdpePrefixed "write_start"))
          cfg.startWriteAt
        Gdb.writeLe @(Unsigned 64)
          gdb
          (getPathAddress MemoryMaps.mu (sdpePrefixed "write_cycles"))
          (numConvert cfg.writeForN)

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
            headBaseAddr = muSwitchDemoPeBuffer
            myBaseAddr = toInteger (headBaseAddr + 24 * (L.length gdbs - num - 1))
            dnaBaseAddr = getPathAddress @Integer MemoryMaps.mu ["0", "Dna", "maybe_dna"]
          myCounter <- Gdb.readLe @(Unsigned 64) headGdb myBaseAddr
          myDeviceDna <- Gdb.readLe @(Maybe (BitVector 96)) myGdb dnaBaseAddr
          headDeviceDna <- Gdb.readLe @(BitVector 96) headGdb (myBaseAddr + 0x08)

          let
            myCfg = configs L.!! num
            expectedCounter = myCfg.startWriteAt
            counterEq = myCounter == expectedCounter
            dnaEq = myDeviceDna == Just headDeviceDna
          unless counterEq
            $ putStrLn
              [i|Counter #{num} did not match. Found #{myCounter}, expected #{expectedCounter}|]
          unless dnaEq
            $ putStrLn
              [i|DNA did not match. Found #{headDeviceDna}, expected #{myDeviceDna}|]
          return $ counterEq && dnaEq

      deviceChecks <- forM (L.zip gdbs [0 ..]) $ \(gdb, n) -> liftIO $ perDeviceCheck gdb n
      let result = if and deviceChecks then ExitSuccess else ExitFailure 2
      return result

  forM_ targets (assertProbe "probe_test_start")

  let
    -- BOOT / MU / CC IDs
    expectedJtagIds = [0x0_514C001, 0x1_514C001, 0x2_514C001]
    toInitArgs (_, deviceInfo) targetIndex =
      Ocd.InitOpenOcdArgs{deviceInfo, expectedJtagIds, hitlDir, targetIndex}
    initArgs = L.zipWith toInitArgs targets [0 ..]
    optionalBootInitArgs = L.repeat def{Ocd.logPrefix = "boot-", Ocd.initTcl = "vexriscv_boot_init.tcl"}
    openOcdBootStarts = liftIO <$> L.zipWith Ocd.initOpenOcd initArgs optionalBootInitArgs

  let picocomStarts = liftIO <$> L.zipWith (initPicocom hitlDir) targets [0 ..]
  brackets picocomStarts (liftIO . snd) $ \(L.map fst -> picocoms) -> do
    -- Start OpenOCD that will program the boot CPU
    brackets openOcdBootStarts (liftIO . (.cleanup)) $ \initOcdsData -> do
      let bootTapInfos = parseBootTapInfo <$> initOcdsData

      Gdb.withGdbs (L.length targets) $ \bootGdbs -> do
        liftIO
          $ zipWithConcurrently3_ (initGdb hitlDir "switch-demo1-boot") bootGdbs bootTapInfos targets
        liftIO $ mapConcurrently_ ((assertEither =<<) . Gdb.loadBinary) bootGdbs
        liftIO $ mapConcurrently_ Gdb.continue bootGdbs
        liftIO
          $ T.tryWithTimeout T.PrintActionTime "Waiting for done" 60_000_000
          $ forConcurrently_ picocoms
          $ \pico ->
            waitForLine pico.stdoutHandle "[BT] Going into infinite loop.."

    let
      optionalInitArgs = L.repeat def
      openOcdStarts = liftIO <$> L.zipWith Ocd.initOpenOcd initArgs optionalInitArgs

    -- Start OpenOCD instances for all CPUs
    brackets openOcdStarts (liftIO . (.cleanup)) $ \initOcdsData -> do
      let
        allTapInfos = parseTapInfo expectedJtagIds <$> initOcdsData

        _bootTapInfos, muTapInfos, ccTapInfos :: [Ocd.TapInfo]
        (_bootTapInfos, muTapInfos, ccTapInfos)
          | all (== L.length expectedJtagIds) (L.length <$> allTapInfos)
          , [boots, mus, ccs] <- L.transpose allTapInfos =
              (boots, mus, ccs)
          | otherwise =
              error
                $ "Unexpected number of OpenOCD taps initialized. Expected: "
                <> show (L.length expectedJtagIds)
                <> ", but got: "
                <> show (L.length <$> allTapInfos)

      Gdb.withGdbs (L.length targets) $ \ccGdbs -> do
        liftIO $ zipWithConcurrently3_ (initGdb hitlDir "clock-control") ccGdbs ccTapInfos targets
        liftIO $ mapConcurrently_ ((assertEither =<<) . Gdb.loadBinary) ccGdbs

        Gdb.withGdbs (L.length targets) $ \muGdbs -> do
          liftIO
            $ zipWithConcurrently3_ (initGdb hitlDir "switch-demo1-mu") muGdbs muTapInfos targets
          liftIO $ mapConcurrently_ ((assertEither =<<) . Gdb.loadBinary) muGdbs

          let goDumpCcSamples = dumpCcSamples hitlDir (defCcConf (natToNum @FpgaCount)) ccGdbs
          liftIO $ mapConcurrently_ Gdb.continue ccGdbs
          liftIO $ mapConcurrently_ Gdb.continue muGdbs

          liftIO
            $ T.tryWithTimeoutOn
              T.PrintActionTime
              "Waiting for captured UGNs"
              60_000_000
              goDumpCcSamples
            $ forConcurrently_ picocoms
            $ \pico ->
              waitForLine pico.stdoutHandle "[MU] All UGNs captured"

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
          -- Continue all MUs so they can continue monitoring
          liftIO $ mapConcurrently_ Gdb.continue muGdbs
          liftIO $ do
            let delayMicros = natToNum @StartDelay * 1_250_000
            threadDelay delayMicros
            liftIO $ mapConcurrently_ Gdb.interrupt muGdbs
            putStrLn [i|Slept for: #{delayMicros}μs|]
            newCurrentTime <- muGetCurrentTime (L.head targets) (L.head muGdbs)
            putStrLn [i|Clock is now: #{newCurrentTime}|]

          _ <- liftIO $ sequenceA $ L.zipWith muReadPeBuffer targets muGdbs

          bufferExit <- finalCheck muGdbs (toList chainConfig)
          liftIO
            $ if bufferExit == ExitSuccess
              then putStrLn "Last PE buffer has expected contents"
              else putStrLn "[ERROR] Last PE buffer did NOT have expected contents"

          ebFlagsClears <- liftIO $ zipWithConcurrently checkElasticBufferFlags targets muGdbs
          unless (L.and ebFlagsClears) $ fail "Some elastic buffers over or underflowed"

          liftIO goDumpCcSamples

          pure bufferExit
