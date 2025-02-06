-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Bittide.Instances.Hitl.Driver.Demo where

import Clash.Prelude

import Bittide.Hitl
import Bittide.Instances.Hitl.Setup (demoRigInfo)
import Bittide.Instances.Hitl.Utils.Program
import Bittide.Instances.Hitl.Utils.Vivado

import Control.Exception (catch)
import Control.Monad (zipWithM)
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
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
import qualified Control.Monad.Trans.Control as CMTC
import qualified Data.List as L
import qualified System.Timeout.Lifted as STL

-- connect EBs over WB
-- read/write regs with MU, ensure that correct values show up
-- check over GDB too
-- no input data
-- just one test case

data OcdInitData = OcdInitData
  { muPort :: Int
  , ccPort :: Int
  , handles :: ProcessStdIoHandles
  , cleanup :: IO ()
  }

data TestStatus = TestRunning | TestDone Bool | TestTimeout deriving (Eq)

driverFunc ::
  String ->
  [(HwTarget, DeviceInfo)] ->
  VivadoM ExitCode
driverFunc testName targets = do
  liftIO
    . putStrLn
    $ "Running driver function for targets "
    <> show ((\(_, info) -> info.deviceId) <$> targets)

  startTime <- liftIO $ getTime Monotonic
  projectDir <- liftIO $ findParentContaining "cabal.project"

  let
    hitlDir = projectDir </> "_build/hitl/" <> testName

    calcTimeSpentMs = (`div` 1_000_000) . toNanoSecs . diffTimeSpec startTime <$> getTime Monotonic

    getTargetIndex :: HwTarget -> Int
    getTargetIndex hwT = fromMaybe 9 $ L.findIndex (\di -> di.deviceId == idFromHwT hwT) demoRigInfo

    tryWithTimeout ::
      forall m a.
      (CMTC.MonadBaseControl IO m, MonadFail m) =>
      String ->
      Int ->
      m a ->
      m a
    tryWithTimeout actionName dur action = do
      result <- STL.timeout dur action
      case result of
        Nothing -> do
          fail $ "Timeout while performing action: " <> actionName
        Just r -> pure r

    getHandshakesStatus :: [(HwTarget, DeviceInfo)] -> [Bool] -> VivadoM [Bool]
    getHandshakesStatus [] _ = return []
    getHandshakesStatus _ [] = return []
    getHandshakesStatus ((hwT, d) : hwtdRest) (handsShaken : hsRest) = do
      let getRest = getHandshakesStatus hwtdRest hsRest
      if handsShaken
        then getRest
        else do
          openHardwareTarget hwT
          vals <- readVio "vioHitlt" ["probe_handshakes_done"]
          let
            getResult =
              case vals of
                [("probe_handshakes_done", "1")] -> do
                  liftIO
                    $ putStrLn
                    $ "!!!!! Handshake completed on device "
                    <> show d.deviceId
                    <> " ("
                    <> show (getTargetIndex hwT)
                    <> ") !!!!!"
                  return True
                _ -> return False
          result <- getResult
          rest <- getRest
          return $ result : rest

    awaitHandshakes :: VivadoM ()
    awaitHandshakes = do
      let
        innerInit = L.repeat False
        inner prev = do
          new <- getHandshakesStatus targets prev
          if and new
            then return ()
            else inner new
      inner innerInit

    initOpenOcds :: (HwTarget, DeviceInfo) -> Int -> IO OcdInitData
    initOpenOcds (_, d) targetIndex = do
      putStrLn $ "Starting OpenOCD for target " <> show d.deviceId

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
        startOpenOcdWithEnvAndArgs
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
      flip
        catch
        ( \(e :: IOError) -> do
            putStrLn $ "Failed on reading OpenOCD stderr: " <> show e
            flip catch (\(_ :: IOError) -> return ()) $ do
              so <- hGetContents ocd.stdoutHandle
              putStrLn $ "OpenOCD leftover stdout:\n" <> so
            flip catch (\(_ :: IOError) -> return ()) $ do
              se <- hGetContents ocd.stderrHandle
              putStrLn $ "OpenOCD leftover stderr:\n" <> se
            fail (show e)
        )
        $ tryWithTimeout "Waiting for OpenOCD to start" 15_000_000
        $ expectLine ocd.stderrHandle openOcdWaitForHalt

      let
        ocdProcName = "OpenOCD (" <> show d.deviceId <> ")"
        ocdClean1 = do
          flip catch (\(e :: IOError) -> putStrLn $ "Failed to read from stdout: " <> show e) $ do
            so <- hGetContents ocd.stdoutHandle
            putStrLn $ "OpenOCD leftover stdout:\n" <> so
          flip catch (\(e :: IOError) -> putStrLn $ "Failed to read from stderr: " <> show e) $ do
            se <- hGetContents ocd.stderrHandle
            putStrLn $ "OpenOCD leftover stderr:\n" <> se
          ocdClean0
          awaitProcessTermination ocdProcName ocdPh (Just 5_000_000)

      return $ OcdInitData gdbPortMU gdbPortCC ocd ocdClean1

    initGdbs :: String -> Int -> (HwTarget, DeviceInfo) -> IO (ProcessStdIoHandles, IO ())
    initGdbs binName gdbPort (hwT, d) = do
      putStrLn $ "Starting GDB for target " <> show d.deviceId <> " with bin name " <> binName

      (gdb, gdbPh, gdbClean0) <- Gdb.startGdbH
      hSetBuffering gdb.stdinHandle LineBuffering

      Gdb.setLogging gdb $ hitlDir
        </> "gdb-" <> binName <> "-" <> show (getTargetIndex hwT) <> ".log"
      Gdb.setFile gdb $ firmwareBinariesDir "riscv32imc" Release </> binName
      Gdb.setTarget gdb gdbPort

      let
        gdbProcName = "GDB (" <> binName <> ", " <> show d.deviceId <> ")"
        gdbClean1 = gdbClean0 >> awaitProcessTermination gdbProcName gdbPh (Just 5_000_000)

      return (gdb, gdbClean1)

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
                    putStrLn $ "Test passed on target " <> show d.deviceId
                    return ExitSuccess
                  TestDone False -> do
                    putStrLn $ "Test finished unsuccessfully on target " <> show d.deviceId
                    return $ ExitFailure 2
                  _ -> do
                    putStrLn $ "Test timed out on target " <> show d.deviceId
                    return $ ExitFailure 2
              liftIO $ zipWithM go targets new
            else inner new
      inner innerInit

    foldExitCodes :: VivadoM (Int, ExitCode) -> ExitCode -> VivadoM (Int, ExitCode)
    foldExitCodes prev code = do
      (count, acc) <- prev
      return
        $ if code == ExitSuccess
          then (count + 1, acc)
          else (count, code)

    muGdbCheck :: (HwTarget, DeviceInfo) -> ProcessStdIoHandles -> VivadoM ExitCode
    muGdbCheck (_, d) gdb = do
      let
        devIdString = show d.deviceId
        expectedDna = showHex d.dna ""
      -- Need to find out what the appropriate invocation is for GDB to get the correct string
      -- out from MMIO.
      liftIO $ Gdb.runCommands gdb.stdinHandle ["x/12sb 0xa0000000", "echo END OF DNA\\n"]
      gdbRead <-
        liftIO
          $ tryWithTimeout "Reading DNA over GDB" 60_000_000
          $ readUntil gdb.stdoutHandle "END OF DNA"
      -- Also need to find out what the output looks like in order to write a proper comparison.
      -- For now, just print what each of them are.
      liftIO $ putStrLn [i|Expected DNA for device #{devIdString}: #{expectedDna}|]
      liftIO $ putStrLn [i|Output from DNA probe:\n#{gdbRead}|]
      return ExitSuccess

  tryWithTimeout "Wait for handshakes successes from all boards" 15_000_000 awaitHandshakes
  brackets (liftIO <$> L.zipWith initOpenOcds targets [0 ..]) (liftIO . (.cleanup)) $ \initOcdsData -> do
    let
      muPorts = (.muPort) <$> initOcdsData
      ccPorts = (.ccPort) <$> initOcdsData
    brackets
      (liftIO <$> L.zipWith (initGdbs "clock-control") ccPorts targets)
      (liftIO . snd)
      $ \initCCGdbsData -> do
        let ccGdbs = fst <$> initCCGdbsData
        liftIO $ mapM_ ((errorToException =<<) . Gdb.loadBinary) ccGdbs
        brackets
          (liftIO <$> L.zipWith (initGdbs "management-unit") muPorts targets)
          (liftIO . snd)
          $ \initMUGdbsData -> do
            let muGdbs = fst <$> initMUGdbsData
            liftIO $ mapM_ ((errorToException =<<) . Gdb.loadBinary) muGdbs

            testResults <- awaitTestCompletions 60_000
            (sCount, stabilityExitCode) <-
              L.foldl foldExitCodes (pure (0, ExitSuccess)) testResults
            liftIO
              $ putStrLn
                [i|Test case #{testName} stabilised on #{sCount} of #{L.length targets} targets|]
            liftIO $ putStrLn "Checking for MMIO access over GDB..."
            gdbExitCodes <- zipWithM muGdbCheck targets ccGdbs
            (gdbCount, gdbExitCode) <-
              L.foldl foldExitCodes (pure (0, ExitSuccess)) gdbExitCodes
            liftIO
              $ putStrLn
                [i|GDB testing passed on #{gdbCount} of #{L.length targets} targets|]
            let
              finalExit =
                fromMaybe ExitSuccess
                  $ L.find (/= ExitSuccess) [stabilityExitCode, gdbExitCode]
            return finalExit
