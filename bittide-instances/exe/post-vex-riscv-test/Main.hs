-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}

import Prelude

import Data.List.Extra (isPrefixOf)
import Data.Maybe (fromJust)
import Paths_bittide_instances
import Project.Handle
import Project.Programs
import System.Environment (withArgs)
import System.IO
import System.Process

import Bittide.Instances.Hitl.Setup (deviceIdSerialPairs)
import Control.Concurrent
import Control.Concurrent.Async (mapConcurrently)
import qualified Data.List as L
import Test.Tasty.HUnit
import Test.Tasty.TH

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
  startOpenOcdPath <- getOpenOcdStartPath
  gdbScriptPath <- getGdbScriptPath

  withAnnotatedGdbScriptPath gdbScriptPath $ \gdbProgPath -> do
    let
      openOcdProc = (proc startOpenOcdPath []){std_err = CreatePipe}
      gdbProc = (proc "gdb" ["--command", gdbProgPath]){std_out = CreatePipe, std_err = CreatePipe}

      -- Wait until we see "Halting processor", fail if we see an error
      waitForHalt s
        | "Error:" `isPrefixOf` s = Stop (Error ("Found error in OpenOCD output: " <> s))
        | "Halting processor" `isPrefixOf` s = Stop Ok
        | otherwise = Continue

    putStrLn "Starting OpenOCD"
    withCreateProcess openOcdProc $ \_ _ (fromJust -> openOcdStdErr) _ -> do
      hSetBuffering openOcdStdErr LineBuffering
      putStrLn "Waiting for OpenOCD to halt the processor..."
      expectLine openOcdStdErr waitForHalt

      uartProcesses <- mapM (startUart . snd) deviceIdSerialPairs
      putStrLn "Starting gdb"
      withCreateProcess gdbProc $ \_ (fromJust -> gdbStdOut) _ _ -> do
        putStrLn "Waiting for GDB to finish the script"
        waitForLine gdbStdOut "> continue"

        putStrLn "Wait a second to receive uart data"
        threadDelay 1_000_000

        putStrLn "Collecting UART data"
        results <- mapConcurrently checkUart uartProcesses

        putStrLn "Results:"
        print $ zipWith (\(deviceId, _) result -> (deviceId, result)) deviceIdSerialPairs results
        let
          result = head $ L.filter (not . L.isInfixOf "read zero bytes from stdin") results
        assertBool "We expect one response" (length result == 1)

checkUart :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO String
checkUart (fromJust -> picoIn, fromJust -> picoOut, _, _) = do
  putStrLn "Writing data"
  hPutStrLn picoIn "Hello FPGA"
  putStrLn "Writing successful"
  threadDelay 1_000_000

  handleOpen <- hIsOpen picoOut
  putStrLn $ "Handle open: " <> show handleOpen
  isEof <- hIsEOF picoOut
  putStrLn $ "Is EOF: " <> show isEof
  if isEof
    then do
      putStrLn "End of file reached"
      pure ""
    else do
      putStrLn "Reading data"
      readRemainingChars picoOut

startUart :: FilePath -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
startUart uartPath = do
  let picocomProc =
        (proc "picocom" ["--baud", "921600", "--imap", "lfcrlf", "--omap", "lfcrlf", uartPath])
          { std_out = CreatePipe
          , std_in = CreatePipe
          , std_err = CreatePipe
          , new_session = True -- Seems to be required for picocom to work
          }
  putStrLn $ "Starting Picocom on " <> uartPath
  process@(maybePicoIn, maybePicoOut, maybePicoErr, _) <- createProcess picocomProc
  let
    picoIn = fromJust maybePicoIn
    picoOut = fromJust maybePicoOut
    picoErr = fromJust maybePicoErr
  hSetBuffering picoIn LineBuffering
  hSetBuffering picoOut LineBuffering
  hSetBuffering picoErr LineBuffering
  waitForLine picoOut "Terminal ready"
  pure process

main :: IO ()
main = withArgs ["--timeout", "2m"] $(defaultMainGenerator)
