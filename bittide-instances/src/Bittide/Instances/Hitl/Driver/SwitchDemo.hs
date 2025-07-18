-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}
-- TODO: Remove use of partial functions
{-# OPTIONS_GHC -Wno-x-partial #-}

module Bittide.Instances.Hitl.Driver.SwitchDemo (
  OcdInitData (..),
  driver,
) where

import Clash.Prelude

import Bittide.Hitl
import Bittide.Instances.Domains
import Bittide.Instances.Hitl.Setup (FpgaCount, fpgaSetup)
import Bittide.Instances.Hitl.Utils.Driver
import Bittide.Instances.Hitl.Utils.Program

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forM, forM_, when, zipWithM)
import Control.Monad.IO.Class
import Data.Bifunctor (Bifunctor (bimap))
import Data.Maybe (fromJust, fromMaybe)
import Data.String.Interpolate (i)
import GHC.Stack (HasCallStack)
import Numeric (showHex)
import Project.FilePath
import Project.Handle
import System.Clock (Clock (Monotonic), diffTimeSpec, getTime, toNanoSecs)
import System.Exit
import System.FilePath
import System.IO
import System.Process (StdStream (CreatePipe, UseHandle))
import Vivado.Tcl (HwTarget)
import Vivado.VivadoM
import "extra" Data.List.Extra (trim)

import qualified Bittide.Calculator as Calc
import qualified Bittide.Instances.Hitl.Utils.Gdb as Gdb
import qualified Bittide.Instances.Hitl.Utils.OpenOcd as Ocd
import qualified Bittide.Instances.Hitl.Utils.Picocom as Picocom
import qualified Clash.Sized.Vector as V (fromList)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.List as L

data OcdInitData = OcdInitData
  { muPort :: Int
  -- ^ Management unit GDB port
  , ccPort :: Int
  -- ^ Clock control GDB port
  , handles :: ProcessStdIoHandles
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

  startTime <- liftIO $ getTime Monotonic
  projectDir <- liftIO $ findParentContaining "cabal.project"

  let
    hitlDir = projectDir </> "_build/hitl" </> testName

    calcTimeSpentMs = (`div` 1_000_000) . toNanoSecs . diffTimeSpec startTime <$> getTime Monotonic

    initOpenOcd :: (HwTarget, DeviceInfo) -> Int -> IO OcdInitData
    initOpenOcd (_, d) targetIndex = do
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

    initGdb :: String -> Int -> (HwTarget, DeviceInfo) -> IO (ProcessStdIoHandles, IO ())
    initGdb binName gdbPort (hwT, d) = do
      putStrLn $ "Starting GDB for target " <> d.deviceId <> " with bin name " <> binName

      (gdb, gdbPh, gdbClean0) <- Gdb.startGdbH
      hSetBuffering gdb.stdinHandle LineBuffering
      hSetBuffering gdb.stdoutHandle LineBuffering

      Gdb.setLogging gdb $ hitlDir
        </> "gdb-" <> binName <> "-" <> show (getTargetIndex hwT) <> ".log"
      Gdb.setFile gdb $ firmwareBinariesDir "riscv32imc" Release </> binName
      Gdb.setTarget gdb gdbPort
      Gdb.setTimeout gdb Nothing
      Gdb.runCommands gdb.stdinHandle ["echo connected to target device"]

      let
        gdbProcName = "GDB (" <> binName <> ", " <> d.deviceId <> ")"
        gdbClean1 = gdbClean0 >> awaitProcessTermination gdbProcName gdbPh (Just 10_000_000)

      return (gdb, gdbClean1)

    initPicocom :: (HwTarget, DeviceInfo) -> Int -> IO (ProcessStdIoHandles, IO ())
    initPicocom (_hwTarget, deviceInfo) targetIndex = do
      -- Using a single handle makes picocom panic
      devNullHandle0 <- openFile "/dev/null" WriteMode
      devNullHandle1 <- openFile "/dev/null" WriteMode

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
              , Picocom.stderr = UseHandle devNullHandle0
              }
          )
          devPath
          stdoutPath
          stderrPath
          []

      tryWithTimeout "Waiting for \"Terminal ready\"" 10_000_000
        $ waitForLine pico.stdoutHandle "Terminal ready"

      -- We're not interested in the output of picocom in this Haskell process,
      -- so we redirect it to /dev/null.
      _ <- forkIO $ do
        LazyByteString.hGetContents pico.stdoutHandle
          >>= LazyByteString.hPut devNullHandle1

      pure (pico, cleanup)

    ccGdbCheck :: (HwTarget, DeviceInfo) -> ProcessStdIoHandles -> VivadoM ExitCode
    ccGdbCheck (_, _) gdb = do
      liftIO
        $ Gdb.runCommands
          gdb.stdinHandle
          ["echo START OF WHOAMI\\n", "x/4cb 0xE0000000", "echo END OF WHOAMI\\n"]
      _ <-
        liftIO
          $ tryWithTimeout "Waiting for GDB to be ready to proceed" 15_000_000
          $ readUntil gdb.stdoutHandle "START OF WHOAMI"
      gdbRead <-
        liftIO
          $ tryWithTimeout "Reading CC whoami over GDB" 15_000_000
          $ readUntil gdb.stdoutHandle "END OF WHOAMI"
      let
        idLine = trim . L.head . lines $ trim gdbRead
        success = idLine == "(gdb) 0xe0000000:\t115 's'\t119 'w'\t99 'c'\t99 'c'"
      liftIO $ putStrLn [i|Output from CC whoami probe:\n#{idLine}|]
      return $ if success then ExitSuccess else ExitFailure 1

    foldExitCodes :: VivadoM (Int, ExitCode) -> ExitCode -> VivadoM (Int, ExitCode)
    foldExitCodes prev code = do
      (count, acc) <- prev
      return
        $ if code == ExitSuccess
          then (count + 1, acc)
          else (count, code)

    assertAllProgrammed :: (HwTarget, DeviceInfo) -> VivadoM ()
    assertAllProgrammed (hwT, d) = do
      liftIO $ putStrLn $ "Asserting all programmed probe on " <> d.deviceId
      openHardwareTarget hwT
      updateVio "vioHitlt" [("probe_all_programmed", "1")]

    muGdbCheck :: (HwTarget, DeviceInfo) -> ProcessStdIoHandles -> VivadoM ExitCode
    muGdbCheck (_, _) gdb = do
      liftIO
        $ Gdb.runCommands
          gdb.stdinHandle
          ["echo START OF WHOAMI\\n", "x/4cb 0xE0000000", "echo END OF WHOAMI\\n"]
      _ <-
        liftIO
          $ tryWithTimeout "Waiting for GDB to be ready to proceed" 15_000_000
          $ readUntil gdb.stdoutHandle "START OF WHOAMI"
      gdbRead <-
        liftIO
          $ tryWithTimeout "Reading MU whoami over GDB" 15_000_000
          $ readUntil gdb.stdoutHandle "END OF WHOAMI"
      let
        idLine = trim . L.head . lines $ trim gdbRead
        success = idLine == "(gdb) 0xe0000000:\t109 'm'\t103 'g'\t109 'm'\t116 't'"
      liftIO $ putStrLn [i|Output from MU whoami probe:\n#{idLine}|]
      return $ if success then ExitSuccess else ExitFailure 1

    getTestsStatus ::
      [(HwTarget, DeviceInfo)] -> [TestStatus] -> Integer -> VivadoM [TestStatus]
    getTestsStatus [] _ _ = return []
    getTestsStatus _ [] _ = return []
    getTestsStatus ((hwT, _) : hwtdRest) (status : statusRest) dur = do
      let getRestStatus = getTestsStatus hwtdRest statusRest dur
      case status of
        TestRunning -> do
          timeSpent <- liftIO $ calcTimeSpentMs
          if timeSpent < dur
            then do
              openHardwareTarget hwT
              vals <- readVio "vioHitlt" ["probe_test_done", "probe_test_success"]
              rest <- getRestStatus
              return $ case vals of
                [("probe_test_done", "1"), ("probe_test_success", success)] ->
                  TestDone (success == "1") : rest
                _ -> TestRunning : rest
            else do
              rest <- getRestStatus
              return $ TestTimeout : rest
        other -> do
          rest <- getRestStatus
          return $ other : rest

    awaitTestCompletions :: Integer -> VivadoM [ExitCode]
    awaitTestCompletions dur = do
      let
        innerInit = L.repeat TestRunning
        inner prev = do
          new <- getTestsStatus targets prev dur
          if not (TestRunning `L.elem` new)
            then do
              let
                go :: (HwTarget, DeviceInfo) -> TestStatus -> IO ExitCode
                go (_, d) status = case status of
                  TestDone True -> do
                    putStrLn $ "Test passed on target " <> d.deviceId
                    return ExitSuccess
                  TestDone False -> do
                    putStrLn $ "Test finished unsuccessfully on target " <> d.deviceId
                    return $ ExitFailure 2
                  _ -> do
                    putStrLn $ "Test timed out on target " <> d.deviceId
                    return $ ExitFailure 2
              liftIO $ zipWithM go targets new
            else inner new
      inner innerInit

    muGetUgns ::
      (HwTarget, DeviceInfo) -> ProcessStdIoHandles -> VivadoM [(Unsigned 64, Unsigned 64)]
    muGetUgns (_, d) gdb = do
      let
        mmioAddrs :: [String]
        mmioAddrs =
          [ "0x10000000"
          , "0x20000000"
          , "0x30000000"
          , "0x40000000"
          , "0x50000000"
          , "0x60000000"
          , "0x70000000"
          ]

        readUgnMmio :: String -> IO (Unsigned 64, Unsigned 64)
        readUgnMmio addr = do
          let
            startString = "START OF UGN (" <> addr <> ")"
            endString = "END OF UGN (" <> addr <> ")"
          Gdb.runCommands
            gdb.stdinHandle
            [ "printf \"" <> startString <> "\\n\""
            , "x/2xg " <> addr
            , "printf \"" <> endString <> "\\n\""
            ]
          _ <-
            tryWithTimeout "Waiting for GDB to be ready for UGN readout" 15_000_000
              $ readUntil gdb.stdoutHandle startString
          gdbRead <-
            tryWithTimeout "Reading UGN over GDB" 15_000_000 $ readUntil gdb.stdoutHandle endString
          let
            outputLine = L.head $ L.lines $ trim gdbRead
            outputDataWords = L.words $ trim $ outputLine
          putStrLn $ "GDB output: {{\n" <> gdbRead <> "}}"
          case L.drop (L.length outputDataWords - 2) outputDataWords of
            [read -> txIJ, read -> rxIJ] -> return (txIJ, rxIJ)
            other -> fail [i|Could not read output data. Line: '#{outputLine}', data: #{other}|]

      liftIO $ putStrLn $ "Getting UGNs for device " <> d.deviceId
      liftIO $ mapM readUgnMmio mmioAddrs

    muReadPeBuffer :: (HasCallStack) => (HwTarget, DeviceInfo) -> ProcessStdIoHandles -> IO ()
    muReadPeBuffer (_, d) gdb = do
      putStrLn $ "Reading PE buffer from device " <> d.deviceId
      let
        startString = "START OF PEBUF (" <> d.deviceId <> ")"
        endString = "END OF PEBUF (" <> d.deviceId <> ")"
        bufferSize :: Integer
        bufferSize = (snatToNum (SNat @FpgaCount)) * 3
      Gdb.runCommands
        gdb.stdinHandle
        [ "printf \"" <> startString <> "\\n\""
        , [i|x/#{bufferSize}xg 0x90000028|]
        , "printf \"" <> endString <> "\\n\""
        ]
      _ <-
        tryWithTimeout "Waiting for GDB to be ready for PE buffer readout" 15_000_000
          $ readUntil gdb.stdoutHandle startString
      bufInit <-
        tryWithTimeout "PE buffer readout" 15_000_000 $ readUntil gdb.stdoutHandle endString
      putStrLn $ "PE buffer readout:\n" <> bufInit

    muGetCurrentTime ::
      (HasCallStack) =>
      (HwTarget, DeviceInfo) ->
      ProcessStdIoHandles ->
      IO (Unsigned 64)
    muGetCurrentTime (_, d) gdb = do
      putStrLn $ "Getting current time from device " <> d.deviceId
      -- Write capture command to `timeWb` component
      Gdb.runCommands gdb.stdinHandle ["set {char[4]}(0xD0000000) = 0x0"]
      let
        currentStringLsbs = "START OF STARTTIME LSBS"
        endStringLsbs = "END OF STARTTIME LSBS"
      Gdb.runCommands
        gdb.stdinHandle
        [ "printf \"" <> currentStringLsbs <> "\\n\""
        , "x/1xw 0xD0000008"
        , "printf \"" <> endStringLsbs <> "\\n\""
        ]
      _ <-
        tryWithTimeout "Waiting for GDB to be ready for curtime lsbs readout" 15_000_000
          $ readUntil gdb.stdoutHandle currentStringLsbs
      currentStringLsbs0 <-
        tryWithTimeout "Current time lsbs readout" 15_000_000
          $ readUntil gdb.stdoutHandle endStringLsbs
      let
        currentStringLsbs1 = trim $ L.head $ lines $ L.drop 2 $ L.dropWhile (/= ':') currentStringLsbs0
        currentTimeLsbsI :: Integer
        currentTimeLsbsI = read currentStringLsbs1
        currentTimeLsbs :: Unsigned 32
        currentTimeLsbs = fromIntegral currentTimeLsbsI
      putStrLn $ "GDB said: " <> show currentStringLsbs1
      putStrLn $ "I read: " <> show currentTimeLsbs
      let
        currentStringMsbs = "START OF CURTIME MSBS"
        endStringMsbs = "END OF CURTIME MSBS"
      Gdb.runCommands
        gdb.stdinHandle
        [ "printf \"" <> currentStringMsbs <> "\\n\""
        , "x/1xw 0xD000000C"
        , "printf \"" <> endStringMsbs <> "\\n\""
        ]
      _ <-
        tryWithTimeout "Waiting for GDB to be ready for curtime msbs readout" 15_000_000
          $ readUntil gdb.stdoutHandle currentStringMsbs
      currentStringMsbs0 <-
        tryWithTimeout "Current time msbs readout" 15_000_000
          $ readUntil gdb.stdoutHandle endStringMsbs
      let
        currentStringMsbs1 = trim $ L.head $ lines $ L.drop 2 $ L.dropWhile (/= ':') currentStringMsbs0
        currentTimeMsbsI :: Integer
        currentTimeMsbsI = read currentStringMsbs1
        currentTimeMsbs :: Unsigned 32
        currentTimeMsbs = fromIntegral currentTimeMsbsI
      putStrLn $ "GDB said: " <> currentStringMsbs1
      putStrLn $ "I read: " <> show currentTimeMsbs
      return $ bitCoerce (currentTimeMsbs, currentTimeLsbs)

    muWriteCfg ::
      (HasCallStack) =>
      (HwTarget, DeviceInfo) ->
      ProcessStdIoHandles ->
      Calc.CyclePeConfig (Unsigned 64) (Index 9) ->
      VivadoM ()
    muWriteCfg target@(_, d) gdb (bimap pack fromIntegral -> cfg) = do
      let
        start = 0x90000000

        write64 :: Unsigned 32 -> BitVector 64 -> [String]
        write64 address value =
          [ [i|set {char[4]}(0x#{addressHex}) = 0x#{valueLsbHex}|]
          , [i|set {char[4]}(0x#{addressNextHex}) = 0x#{valueMsbHex}|]
          ]
         where
          addressHex = showHex address ""
          addressNextHex = showHex (address + 4) ""
          valueLsbHex = showHex valueLsb ""
          valueMsbHex = showHex valueMsb ""
          (valueMsb :: BitVector 32, valueLsb :: BitVector 32) = bitCoerce value

      liftIO $ do
        muReadPeBuffer target gdb
        putStrLn $ "Writing config to device " <> d.deviceId
        Gdb.runCommands
          gdb.stdinHandle
          ( L.concat
              [ write64 (start + 0) cfg.startReadAt
              , write64 (start + 8) cfg.readForN
              , write64 (start + 16) cfg.startWriteAt
              , write64 (start + 24) cfg.writeForN
              ]
          )

    finalCheck ::
      (HasCallStack) =>
      [ProcessStdIoHandles] ->
      [Calc.CyclePeConfig (Unsigned 64) (Index 9)] ->
      VivadoM ExitCode
    finalCheck [] _ = fail "Should pass in two or more GDBs for the final check!"
    finalCheck (_ : []) _ = fail "Should pass in two or more GDBs for the final check!"
    finalCheck gdbs@(headGdb : _) configs = do
      let
        hexEq :: String -> String -> Bool
        hexEq a b = drop0sA == drop0sB
         where
          drop0xA =
            if "0x" `L.isPrefixOf` a
              then L.drop 2 a
              else a
          drop0xB =
            if "0x" `L.isPrefixOf` b
              then L.drop 2 b
              else b
          drop0sA = L.dropWhile (== '0') drop0xA
          drop0sB = L.dropWhile (== '0') drop0xB
        perDeviceCheck :: ProcessStdIoHandles -> Int -> IO Bool
        perDeviceCheck myGdb num = do
          let
            headBaseAddr = 0x90000028
            myBaseAddr = headBaseAddr + 24 * (L.length gdbs - num - 1)
          myCounter <-
            readSingleGdbValue headGdb ("COUNTER-" <> show num) ("x/1gx 0x" <> showHex myBaseAddr "")
          myDeviceDna2 <- readSingleGdbValue myGdb ("DNA2-" <> show num) "x/1wx 0xB0000000"
          myDeviceDna1 <- readSingleGdbValue myGdb ("DNA1-" <> show num) "x/1wx 0xB0000004"
          myDeviceDna0 <- readSingleGdbValue myGdb ("DNA0-" <> show num) "x/1wx 0xB0000008"
          headDeviceDna2 <-
            readSingleGdbValue
              headGdb
              ("DNA2-" <> show num)
              ("x/1wx 0x" <> showHex (myBaseAddr + 0x08) "")
          headDeviceDna1 <-
            readSingleGdbValue
              headGdb
              ("DNA2-" <> show num)
              ("x/1wx 0x" <> showHex (myBaseAddr + 0x0C) "")
          headDeviceDna0 <-
            readSingleGdbValue
              headGdb
              ("DNA2-" <> show num)
              ("x/1wx 0x" <> showHex (myBaseAddr + 0x10) "")
          let
            myCfg = configs L.!! num
            expectedCounter = showHex myCfg.startWriteAt ""
            counterEq = hexEq myCounter expectedCounter
            dna0Eq = hexEq myDeviceDna0 headDeviceDna0
            dna1Eq = hexEq myDeviceDna1 headDeviceDna1
            dna2Eq = hexEq myDeviceDna2 headDeviceDna2
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

  liftIO
    $ putStrLn
      "All programmed pin may be asserted from previous test - deasserting on all targets."
  forM_ targets (deassertProbe "probe_all_programmed")
  forM_ targets (assertProbe "probe_test_start")
  tryWithTimeout "Wait for handshakes successes from all boards" 30_000_000
    $ awaitHandshakes targets
  let openOcdStarts = liftIO <$> L.zipWith initOpenOcd targets [0 ..]
  brackets openOcdStarts (liftIO . (.cleanup)) $ \initOcdsData -> do
    let
      muPorts = (.muPort) <$> initOcdsData
      ccPorts = (.ccPort) <$> initOcdsData
      ccGdbStarts = liftIO <$> L.zipWith (initGdb "clock-control") ccPorts targets
    brackets ccGdbStarts (liftIO . snd) $ \initCCGdbsData -> do
      let ccGdbs = fst <$> initCCGdbsData
      liftIO $ putStrLn "Checking for MMIO access to SwCC CPUs over GDB..."
      gdbExitCodes0 <- zipWithM ccGdbCheck targets ccGdbs
      (gdbCount0, gdbExitCode0) <-
        L.foldl foldExitCodes (pure (0, ExitSuccess)) gdbExitCodes0
      liftIO
        $ putStrLn
          [i|CC GDB testing passed on #{gdbCount0} of #{L.length targets} targets|]
      liftIO $ mapM_ ((errorToException =<<) . Gdb.loadBinary) ccGdbs
      let muGdbStarts = liftIO <$> L.zipWith (initGdb "management-unit") muPorts targets
      brackets muGdbStarts (liftIO . snd) $ \initMUGdbsData -> do
        let muGdbs = fst <$> initMUGdbsData
        liftIO $ putStrLn "Checking for MMIO access to MU CPUs over GDB..."
        gdbExitCodes1 <- zipWithM muGdbCheck targets muGdbs
        (gdbCount1, gdbExitCode1) <-
          L.foldl foldExitCodes (pure (0, ExitSuccess)) gdbExitCodes1
        liftIO
          $ putStrLn
            [i|MU GDB testing passed on #{gdbCount1} of #{L.length targets} targets|]
        liftIO $ mapM_ ((errorToException =<<) . Gdb.loadBinary) muGdbs

        let picocomStarts = liftIO <$> L.zipWith (initPicocom) targets [0 ..]
        brackets picocomStarts (liftIO . snd) $ \_picocoms -> do
          liftIO $ mapM_ Gdb.continue ccGdbs
          -- XXX: If you also want to start the management units, uncomment this
          --      next line. Beware that this breaks the demo, because we want
          --      to take over control of the management unit again to read out
          --      various values, but we don't have a way to pause the CPU again.
          --
          -- TODO: Add a way to send SIGINT to a GDB process.
          -- liftIO $ mapM_ Gdb.continue muGdbs
          forM_ targets assertAllProgrammed
          testResults <- awaitTestCompletions 60_000
          (sCount, stabilityExitCode) <-
            L.foldl foldExitCodes (pure (0, ExitSuccess)) testResults
          liftIO
            $ putStrLn
              [i|Test case #{testName} stabilised on #{sCount} of #{L.length targets} targets|]

          liftIO $ putStrLn "Getting UGNs for all targets"
          ugnPairsTable <- zipWithM muGetUgns targets muGdbs
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

          let
            finalExit =
              fromMaybe ExitSuccess
                $ L.find (/= ExitSuccess) [stabilityExitCode, gdbExitCode0, gdbExitCode1, bufferExit]
          return finalExit
