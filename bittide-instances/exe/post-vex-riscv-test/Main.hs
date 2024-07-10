-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

import Prelude

import Data.List.Extra (isPrefixOf)
import Data.Maybe (fromJust)
import Paths_bittide_instances
import Project.Handle
import Project.Programs
import System.Environment (withArgs)
import System.IO
import System.Process

import Test.Tasty.HUnit
import Test.Tasty.TH

getGdbScriptPath :: IO FilePath
getGdbScriptPath = getDataFileName "data/gdb/test-gdb-prog"

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
  startPicocomPath <- getPicocomStartPath
  uartDev <- getUartDev
  gdbScriptPath <- getGdbScriptPath

  withAnnotatedGdbScriptPath gdbScriptPath $ \gdbProgPath -> do
    let
      openOcdProc = (proc startOpenOcdPath []){std_err = CreatePipe}
      picocomProc = (proc startPicocomPath [uartDev]){std_out = CreatePipe, std_in = CreatePipe}
      gdbProc = (proc "gdb" ["--command", gdbProgPath]){std_out = CreatePipe, std_err = CreatePipe}

      -- Wait until we see "Halting processor", fail if we see an error
      waitForHalt s
        | "Error:" `isPrefixOf` s = Stop (Error ("Found error in OpenOCD output: " <> s))
        | "Halting processor" `isPrefixOf` s = Stop Ok
        | otherwise = Continue

    withCreateProcess openOcdProc $ \_ _ (fromJust -> openOcdStdErr) _ -> do
      hSetBuffering openOcdStdErr LineBuffering
      expectLine openOcdStdErr waitForHalt

      -- XXX: Picocom doesn't immediately clean up after closing, because it
      --      spawns as a child of the shell (start.sh). We could use 'exec' to
      --      make sure the intermediate shell doesn't exist, but this causes
      --      the whole test program to exit with signal 15 (??????).
      withCreateProcess picocomProc $ \maybePicocomStdIn maybePicocomStdOut _ _ -> do
        let
          picocomStdIn = fromJust maybePicocomStdIn
          picocomStdOut = fromJust maybePicocomStdOut

        hSetBuffering picocomStdIn LineBuffering
        hSetBuffering picocomStdOut LineBuffering

        waitForLine picocomStdOut "Terminal ready"

        withCreateProcess gdbProc $ \_ (fromJust -> gdbStdOut) _ _ -> do
          -- Wait for GDB to program the FPGA. If successful, we should see
          -- "going in echo mode" in the picocom output.
          hSetBuffering gdbStdOut LineBuffering
          waitForLine picocomStdOut "Going in echo mode!"

          -- Wait for GDB to reach its last command - where it will wait indefinitely
          waitForLine gdbStdOut "> continue"

          -- Test UART echo
          hPutStrLn picocomStdIn "Hello, UART!"
          waitForLine picocomStdOut "Hello, UART!"

main :: IO ()
main = withArgs ["--timeout", "2m"] $(defaultMainGenerator)
