-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Prelude

import Control.Concurrent (threadDelay)
import Control.Exception (catch)
import Data.Maybe (fromJust)
import System.Environment (withArgs)
import System.IO
import System.Posix.Env (getEnvironment)
import System.Process
import System.Timeout

import Test.Tasty.HUnit
import Test.Tasty.TH

import Bittide.Instances.Hitl.Setup
import Control.Monad (forM)
import Paths_bittide_instances
import Project.Handle
import Project.Programs

getGdbScriptPath :: IO FilePath
getGdbScriptPath = getDataFileName "data/gdb/test-gdb-prog.gdb"

{- | Test that the GDB program works as expected. This test will start OpenOCD,
Picocom, and GDB, and will wait for the GDB program to execute specific
commands. This test will fail if any of the processes fail, or if the GDB
program does not execute the expected commands.

OpenOCD: A program that communicates with the FPGA over JTAG. When it starts
         it will \"interrogate\" the JTAG chain - making sure it can read our
         CPU's ID. After that, it will open a GDB server on port 3333.

Picocom: A program that communicates with the FPGA over UART.

GDB: GNU Debugger. This program will connect to the OpenOCD server and is able
     to, amongst other things, load programs, set break points, and step
     through code.
-}
case_testGdbProgram :: Assertion
case_testGdbProgram = do
  successes <- forM demoRigInfo $ \d ->
    catch
      (testGdb d >> pure True)
      (\e -> catchHUnitFailure d e >> pure False)

  print successes
  assertBool "Not all tests succeeded. See logs for more information." (and successes)
 where
  -- We use this solution so the test stops when it encounters a problem
  catchHUnitFailure :: DeviceInfo -> HUnitFailure -> IO ()
  catchHUnitFailure d (HUnitFailure _ msg) = do
    putStrLn $ "Test failed on device " <> d.deviceId <> " with: " <> msg

  testGdb :: DeviceInfo -> IO ()
  testGdb d = do
    startOpenOcdPath <- getOpenOcdStartPath
    startPicocomPath <- getPicocomStartPath
    gdbScriptPath <- getGdbScriptPath
    putStrLn $
      "Testing for device " <> d.deviceId <> " with JTAG location " <> d.usbAdapterLocation

    withAnnotatedGdbScriptPath gdbScriptPath $ \gdbProgPath -> do
      currentEnv <- getEnvironment
      let
        openOcdProc =
          (proc startOpenOcdPath [])
            { env = Just (currentEnv <> [("USB_DEVICE", d.usbAdapterLocation)])
            , std_err = CreatePipe
            }
        picocomProc =
          (proc startPicocomPath [d.serial])
            { std_out = CreatePipe
            , std_in = CreatePipe
            , new_session = True
            }
        gdbProc = (proc "gdb" ["--command", gdbProgPath]){std_out = CreatePipe, std_err = CreatePipe}

      withCreateProcess openOcdProc $ \_ _ (fromJust -> openOcdStdErr) _ -> do
        hSetBuffering openOcdStdErr LineBuffering
        expectLine openOcdStdErr waitForHalt

        -- XXX: Picocom doesn't immediately clean up after closing, because it
        --      spawns as a child of the shell (start.sh). We could use 'exec' to
        --      make sure the intermediate shell doesn't exist, but this causes
        --      the whole test program to exit with signal 15 (??????).
        withCreateProcess picocomProc $ \maybePicocomStdIn maybePicocomStdOut maybePicocomStdErr _ -> do
          let
            picocomStdIn = fromJust maybePicocomStdIn
            picocomStdOut = fromJust maybePicocomStdOut

            -- Create function to log the output of the processes
            loggingSequence = do
              threadDelay 1_000_000 -- Wait 1 second for data loggers to catch up
              putStrLn "Picocom stdout"
              picocomOut <- readRemainingChars picocomStdOut
              putStrLn picocomOut
              case maybePicocomStdErr of
                Nothing -> pure ()
                Just h -> do
                  putStrLn "Picocom StdErr"
                  readRemainingChars h >>= putStrLn

            tryWithTimeout :: String -> Int -> IO a -> IO a
            tryWithTimeout actionName dur action = do
              result <- timeout dur action
              case result of
                Nothing -> do
                  loggingSequence
                  assertFailure $ "Timeout while performing action: " <> actionName
                Just r -> pure r

          hSetBuffering picocomStdIn LineBuffering
          hSetBuffering picocomStdOut LineBuffering

          tryWithTimeout "Waiting for \"Terminal ready\"" 10_000_000 $
            waitForLine picocomStdOut "Terminal ready"

          withCreateProcess gdbProc $ \_ (fromJust -> gdbStdOut) _ _ -> do
            -- Wait for GDB to program the FPGA. If successful, we should see
            -- "going in echo mode" in the picocom output.
            hSetBuffering gdbStdOut LineBuffering
            tryWithTimeout "Waiting for \"Going in echo mode!\"" 10_000_000 $
              waitForLine picocomStdOut "Going in echo mode!"

            -- Wait for GDB to reach its last command - where it will wait indefinitely
            tryWithTimeout "Waiting for \"> continue\"" 10_000_000 $
              waitForLine gdbStdOut "> continue"

            -- Test UART echo
            hPutStrLn picocomStdIn "Hello, UART!"
            tryWithTimeout "Waiting for \"Hello, UART!\"" 10_000_000 $
              waitForLine picocomStdOut "Hello, UART!"

main :: IO ()
main = withArgs ["--timeout", "2m"] $(defaultMainGenerator)
