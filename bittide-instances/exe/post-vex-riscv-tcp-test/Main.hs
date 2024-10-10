-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}

import Prelude

import Data.List.Extra
import Data.Maybe
import Paths_bittide_instances
import Project.Handle
import Project.Programs
import System.Environment (withArgs)
import System.IO
import System.Process
import Test.Tasty.HUnit
import Test.Tasty.TH

getGdbScriptPath :: IO FilePath
getGdbScriptPath = getDataFileName "data/gdb/smoltcp-hitl-prog.gdb"

{- | Return the beginning of a string until you detect a certain substring
That substring is not included in the result.
-}
readUntil :: String -> String -> String
readUntil end inp
  | isPrefixOf end inp = ""
  | otherwise = case uncons inp of
      Just (h, t) -> h : readUntil end t
      Nothing -> ""

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
  startTcpPrayhPath <- getTcpSprayPath
  uartDev <- getUartDev
  gdbScriptPath <- getGdbScriptPath

  putStrLn "Starting Gdb"
  withAnnotatedGdbScriptPath gdbScriptPath $ \gdbProgPath -> do
    let
      openOcdProc = (proc startOpenOcdPath []){std_err = CreatePipe}
      tcpSprayProc = (proc startTcpPrayhPath []){std_out = CreatePipe, std_err = CreatePipe}
      gdbProc = (proc "gdb" ["--command", gdbProgPath]){std_out = CreatePipe, std_err = CreatePipe}
      picocomProc = (proc startPicocomPath [uartDev]){std_out = CreatePipe, std_in = CreatePipe}

      -- Wait until we see "Halting processor", fail if we see an error
      waitForHalt s
        | "Error:" `isPrefixOf` s = Stop (Error ("Found error in OpenOCD output: " <> s))
        | "Halting processor" `isPrefixOf` s = Stop Ok
        | otherwise = Continue

    putStrLn "Starting OpenOcd..."
    withCreateProcess openOcdProc $ \_ _ (fromJust -> openOcdStdErr) _ -> do
      hSetBuffering openOcdStdErr LineBuffering

      putStr "Waiting for halt..."
      expectLine openOcdStdErr waitForHalt
      putStrLn " Done"

      putStrLn "Starting Picocom..."
      withCreateProcess picocomProc $ \maybePicocomStdIn maybePicocomStdOut maybePicocomStdErr _ -> do
        let
          picocomStdInHandle = fromJust maybePicocomStdIn
          picocomStdOutHandle = fromJust maybePicocomStdOut

        hSetBuffering picocomStdInHandle LineBuffering
        hSetBuffering picocomStdOutHandle LineBuffering

        putStr "Waiting for Picocom to be ready..."
        hFlush stdout
        waitForLine picocomStdOutHandle "Terminal ready"
        putStrLn " Done"

        putStrLn "Starting GDB..."
        withCreateProcess gdbProc $ \_ (fromJust -> gdbStdOut) _ _ -> do
          -- Wait for GDB to program the FPGA. If successful, we should see
          -- "going in echo mode" in the picocom output.
          hSetBuffering gdbStdOut LineBuffering

          putStr "Waiting for GDB to program the FPGA..."
          hFlush stdout
          waitForLine picocomStdOutHandle "listening"
          putStrLn " Done"

        putStrLn "Starting TCP Spray..."
        -- Test TCP echo server
        withCreateProcess tcpSprayProc $ \_ (fromJust -> tcpStdOut) (fromJust -> tcpStdErr) _ -> do
          (stdOutContents, stdErrContents) <- do
            stdOutContents' <- hGetContents' tcpStdOut
            stdErrContents' <- hGetContents' tcpStdErr
            pure (stdOutContents', stdErrContents')
          picocomOut <- hGetContents picocomStdOutHandle

          case maybePicocomStdErr of
            Nothing -> pure ()
            Just h -> do
              putStrLn "Picocom StdErr"
              readRemainingChars h >>= putStrLn

          putStrLn ""
          putStrLn "Picocom stdout"
          putStrLn $ readUntil "listening" picocomOut

          putStrLn ""
          putStrLn "tcpSpray StdOut"
          putStrLn stdOutContents

          if not (null stdErrContents)
            then assertFailure $ "tcpspray failed with stderr:\n" <> stdErrContents
            else pure ()

main :: IO ()
main = withArgs ["--timeout", "2m"] $(defaultMainGenerator)
