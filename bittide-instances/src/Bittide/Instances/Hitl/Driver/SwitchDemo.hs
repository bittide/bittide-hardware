-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Bittide.Instances.Hitl.Driver.SwitchDemo (
  AdaptersInitData (..),
  ccWhoAmI,
  driver,
  muWhoAmI,
  whoAmIPfx,
) where

import Clash.Prelude

import Bittide.Hitl
import Bittide.Instances.Domains
import Bittide.Instances.Hitl.Setup (FpgaCount, fpgaSetup)
import Bittide.Instances.Hitl.Utils.Driver
import Bittide.Instances.Hitl.Utils.Program

import Clash.Sized.Extra (extendLsb0s)
import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, unless, zipWithM, (<=<))
import Control.Monad.IO.Class
import Data.Bifunctor (Bifunctor (bimap))
import Data.List.Extra (trim)
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
import Vivado.Tcl (HwTarget)
import Vivado.VivadoM

import qualified Bittide.Instances.Hitl.Utils.Gdb as Gdb
import qualified Bittide.SwitchDemoProcessingElement.Calculator as Calc
import qualified Clash.Sized.Vector as V (fromList)
import Data.Foldable (sequenceA_)
import qualified Data.List as L

whoAmIPfx :: forall n. (KnownNat n) => Unsigned (n + 3)
whoAmIPfx = extendLsb0s @3 @n (0b111 :: Unsigned 3)

muWhoAmI :: BitVector 32
muWhoAmI = 0x746d_676d

ccWhoAmI :: BitVector 32
ccWhoAmI = 0x6363_7773

data AdaptersInitData = AdaptersInitData
  { muPort :: Int
  -- ^ Management unit GDB port
  , ccPort :: Int
  -- ^ Clock control GDB port
  , handles :: ProcessStdIoHandles
  -- ^ gdb-adapters stdio handles
  , cleanup :: IO ()
  -- ^ Cleanup function
  }

data TestStatus = TestRunning | TestDone Bool | TestTimeout deriving (Eq)

type StartDelay = 5 -- seconds

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
  startTime <- liftIO $ getTime Monotonic

  let
    hitlDir = projectDir </> "_build/hitl" </> testName

    calcTimeSpentMs = (`div` 1_000_000) . toNanoSecs . diffTimeSpec startTime <$> getTime Monotonic

    initGdbAdapters :: (HwTarget, DeviceInfo) -> Int -> IO AdaptersInitData
    initGdbAdapters (_, d) targetIndex = do
      putStrLn $ "Starting gdb-adapters for target " <> d.deviceId

      let
        gdbPortMU = 3333 + targetIndex * 2
        gdbPortCC = gdbPortMU + 1
        adapterStdout = hitlDir </> "gdb-adapters-" <> show targetIndex <> "-stdout.log"
        adapterStderr = hitlDir </> "gdb-adapters-" <> show targetIndex <> "-stderr.log"
        adapterConfig =
          GdbAdaptersConfig
            { usbDev = d.usbAdapterLocation
            , memMapAddress = whoAmIPfx
            , cpuMap = Build [(muWhoAmI, gdbPortMU), (ccWhoAmI, gdbPortCC)]
            , stdoutPath = Just adapterStdout
            , stderrPath = Just adapterStderr
            }
      putStrLn $ "logging gdb-adapters stdout to `" <> adapterStdout <> "`"
      putStrLn $ "logging gdb-adapters stderr to `" <> adapterStderr <> "`"

      putStrLn "Starting gdb-adapters..."
      (adapter, adapterPh, adapterClean0) <- startGdbAdapters adapterConfig
      hSetBuffering adapter.stderrHandle LineBuffering
      tryWithTimeout "Waiting for gdb-adapters to start" 15_000_000
        $ expectLine adapter.stderrHandle adaptersWaitForHalt

      let
        adapterProcName = "gdb-adapters (" <> d.deviceId <> ")"
        adapterClean1 = adapterClean0 >> awaitProcessTermination adapterProcName adapterPh (Just 10_000_000)

      return $ AdaptersInitData gdbPortMU gdbPortCC adapter adapterClean1

    initGdbs :: String -> Int -> (HwTarget, DeviceInfo) -> IO (ProcessStdIoHandles, IO ())
    initGdbs binName gdbPort (hwT, d) = do
      putStrLn $ "Starting GDB for target " <> d.deviceId <> " with bin name " <> binName

      (gdb, gdbPh, gdbClean0) <- Gdb.startGdbH
      hSetBuffering gdb.stdinHandle LineBuffering
      hSetBuffering gdb.stdoutHandle LineBuffering

      Gdb.setLogging gdb $ hitlDir
        </> "gdb-" <> binName <> "-" <> show (getTargetIndex hwT) <> ".log"
      Gdb.setFile gdb $ firmwareBinariesDir RiscV Release </> binName
      Gdb.setTarget gdb gdbPort
      Gdb.setTimeout gdb Nothing
      Gdb.runCommands gdb.stdinHandle ["echo connected to target device"]

      let
        gdbProcName = "GDB (" <> binName <> ", " <> d.deviceId <> ")"
        gdbClean1 = gdbClean0 >> awaitProcessTermination gdbProcName gdbPh (Just 10_000_000)

      return (gdb, gdbClean1)

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

    getTestsStatus ::
      [(HwTarget, DeviceInfo)] -> [TestStatus] -> Integer -> VivadoM [TestStatus]
    getTestsStatus [] _ _ = return []
    getTestsStatus _ [] _ = return []
    getTestsStatus ((hwT, _) : hwtdRest) (status : statusRest) dur = do
      let getRestStatus = getTestsStatus hwtdRest statusRest dur
      case status of
        TestRunning -> do
          timeSpent <- liftIO calcTimeSpentMs
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
          if TestRunning `notElem` new
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
            outputDataWords = L.words $ trim outputLine
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
        bufferSize = snatToNum (SNat @FpgaCount) * 3
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
      Calc.PeConfig (Unsigned 64) (Index 9) ->
      VivadoM ()
    muWriteCfg target@(_, d) gdb (bimap pack fromIntegral -> cfg) = do
      let
        start = 0x9000_0000

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
      [Calc.PeConfig (Unsigned 64) (Index 9)] ->
      VivadoM ExitCode
    finalCheck [] _ = fail "Should pass in two or more GDBs for the final check!"
    finalCheck [_] _ = fail "Should pass in two or more GDBs for the final check!"
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
            headBaseAddr = 0x9000_0028
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
          unless counterEq
            $ putStrLn
              [i|Counter #{num} did not match. Found #{myCounter}, expected #{expectedCounter}|]
          unless dna0Eq
            $ putStrLn
              [i|DNA0 #{num} did not match. Found #{headDeviceDna0}, expected #{myDeviceDna0}|]
          unless dna1Eq
            $ putStrLn
              [i|DNA1 #{num} did not match. Found #{headDeviceDna1}, expected #{myDeviceDna1}|]
          unless dna2Eq
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
  brackets
    (liftIO <$> L.zipWith initGdbAdapters targets [0 ..])
    (liftIO . (.cleanup))
    $ \initAdaptersData ->
      do
        let
          muPorts = (.muPort) <$> initAdaptersData
          ccPorts = (.ccPort) <$> initAdaptersData
        brackets
          (liftIO <$> L.zipWith (initGdbs "clock-control") ccPorts targets)
          (liftIO . snd)
          $ \initCCGdbsData -> do
            let ccGdbs = fst <$> initCCGdbsData
            liftIO $ mapM_ (errorToException <=< Gdb.loadBinary) ccGdbs
            brackets
              (liftIO <$> L.zipWith (initGdbs "management-unit") muPorts targets)
              (liftIO . snd)
              $ \initMUGdbsData -> do
                let muGdbs = fst <$> initMUGdbsData
                liftIO $ mapM_ (errorToException <=< Gdb.loadBinary) muGdbs

                liftIO $ mapM_ Gdb.continue ccGdbs
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
                  chainConfig :: Vec FpgaCount (Calc.PeConfig (Unsigned 64) (Index 9))
                  chainConfig =
                    Calc.fullChainConfiguration
                      (SNat @3)
                      fpgaSetup
                      ugnPairsTableV
                      startOffset
                liftIO $ do
                  putStrLn [i|Current clock cycle: #{currentTime}|]
                  putStrLn "Calculated the following configs for the switch processing elements:"
                  forM_ chainConfig print
                sequenceA_ $ L.zipWith3 muWriteCfg targets muGdbs (toList chainConfig)
                liftIO $ do
                  let delayMicros = natToNum @StartDelay * 1_250_000
                  threadDelay delayMicros
                  putStrLn [i|Slept for: #{delayMicros}μs|]
                  newCurrentTime <- muGetCurrentTime (L.head targets) (L.head muGdbs)
                  putStrLn [i|Clock is now: #{newCurrentTime}|]

                _ <- liftIO $ zipWithM muReadPeBuffer targets muGdbs

                bufferExit <- finalCheck muGdbs (toList chainConfig)

                let
                  finalExit =
                    fromMaybe ExitSuccess $ L.find (/= ExitSuccess) [stabilityExitCode, bufferExit]
                return finalExit
