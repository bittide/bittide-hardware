-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NumericUnderscores #-}

import Prelude

import Paths_bittide_instances

import Control.Monad (unless)
import Control.Monad.Extra (forM_)
import Data.Maybe
import System.Environment (withArgs)
import System.IO
import System.IO.Temp
import System.Process
import Test.Tasty.HUnit
import Test.Tasty.TH
import Data.List.Extra

data Error = Ok | Error String
data Filter = Continue | Stop Error

getOpenOcdStartPath :: IO FilePath
getOpenOcdStartPath = getDataFileName "data/openocd/start.sh"

getGdbProgPath :: IO FilePath
getGdbProgPath = getDataFileName "data/gdb/smoltcp-hitl-prog"

getTcpSprayPath :: IO FilePath
getTcpSprayPath = getDataFileName "data/tcpspray/start.sh"

getPicocomStartPath :: IO FilePath
getPicocomStartPath = getDataFileName "data/picocom/start.sh"

-- | XXX: Currently hardcoded to a very specific position. Maybe we could probe
--        using JTAG to see what device we're connected to?
getUartDev :: IO String
getUartDev = pure "/dev/serial/by-path/pci-0000:00:14.0-usb-0:5.1:1.1-port0"

-- | Copy the GDB program obtained from 'getGdbProgPath' to a temporary file,
-- prepend each non-comment, non-empty line with 'echo > {line}\n'. This effectively
-- emulates Bash's 'set -x' for the GDB program. This can in turn be used to
-- wait for specific commands to be executed, or simply for debugging.
withAnnotatedGdbProgPath :: (String -> IO ()) -> IO ()
withAnnotatedGdbProgPath action = do
  srcPath <- getGdbProgPath
  withSystemTempFile "test-gdb-prog" $ \dstPath dstHandle -> do
    withFile srcPath ReadMode $ \srcHandle -> do
      srcLines <- lines <$> hGetContents srcHandle
      forM_ srcLines $ \line -> do
        let trimmedLine = trim line
        unless
          (null trimmedLine || "#" `isPrefixOf` trimmedLine)
          (  hPutStr dstHandle "echo > "
          >> hPutStr dstHandle line
          >> hPutStrLn dstHandle "\\n" )
        hPutStrLn dstHandle line

    hClose dstHandle
    action dstPath

-- | Utility function that reads lines from a handle, and applies a filter to
-- each line. If the filter returns 'Continue', the function will continue
-- reading lines. If the filter returns @Stop Ok@, the function will return
-- successfully. If the filter returns @Stop (Error msg)@, the function will
-- fail with the given message.
expectLine :: HasCallStack => Handle -> (String -> Filter) -> Assertion
expectLine h f = do
  line <- trim <$> hGetLine h
  let cont = expectLine h f
  if null line
  then cont
  else case f line of
    Continue -> cont
    Stop Ok -> pure ()
    Stop (Error msg) -> assertFailure msg

-- | Utility function that reads lines from a handle, and waits for a specific
-- line to appear. Though this function does not fail in the traditional sense,
-- it will get stuck if the expected line does not appear. Only use in combination
-- with sensible time outs (also see 'main').
waitForLine :: Handle -> String -> IO ()
waitForLine h expected =
  expectLine h $ \s ->
    if s == expected
    then Stop Ok
    else Continue

-- | Return the beginnig of a string untill you detect a certain substring
readUntil :: String -> String -> String
readUntil end inp
  | isPrefixOf end inp = ""
  | otherwise          = case uncons inp of
    Just (h, t) -> h : readUntil end t
    Nothing -> ""

readRemainingChars :: Handle -> IO String
readRemainingChars h = do
  hSetBuffering h NoBuffering
  rdy <- hReady h
  if rdy
    then do
      c <- hGetChar h
      (c :) <$> readRemainingChars h
    else (pure "")

readRemainingLines :: Handle -> IO [String]
readRemainingLines h = do
  hSetBuffering h LineBuffering
  rdy <- hReady h
  if rdy
    then do
      c <- hGetLine h
      (c:) <$>  readRemainingLines h
    else pure []

readRemaining :: Handle -> IO String
readRemaining h = do
  remLines <- readRemainingLines h
  remChars <- readRemainingChars h
  pure $ unlines remLines <> remChars

-- | Test that the GDB program works as expected. This test will start OpenOCD,
-- Picocom, and GDB, and will wait for the GDB program to execute specific
-- commands. This test will fail if any of the processes fail, or if the GDB
-- program does not execute the expected commands.
--
-- OpenOCD: A program that communicates with the FPGA over JTAG. When it starts
--          it will \"interrogate\" the JTAG chain - making sure it can read our
--          CPU's ID. After that, it will open a GDB server on port 3333.
--
-- Picocom: A program that communicates with the FPGA over UART.
--
-- GDB: GNU Debugger. This program will connect to the OpenOCD server and is able
--      to, amongst other things, load programs, set break points, and step
--      through code.
--
case_testGdbProgram :: Assertion
case_testGdbProgram = do
  startOpenOcdPath <- getOpenOcdStartPath
  startPicocomPath <- getPicocomStartPath
  startTcpPrayhPath <- getTcpSprayPath
  uartDev <- getUartDev

  putStrLn "Starting Gdb"
  withAnnotatedGdbProgPath $ \gdbProgPath -> do
    let
      openOcdProc = (proc startOpenOcdPath []){std_err=CreatePipe}
      tcpSprayProc = (proc startTcpPrayhPath []){std_out = CreatePipe, std_err=CreatePipe}
      gdbProc = (proc "gdb" ["--command", gdbProgPath]){std_out = CreatePipe, std_err=CreatePipe}
      picocomProc = (proc startPicocomPath [uartDev]){std_out=CreatePipe, std_in=CreatePipe}

      -- Wait until we see "Halting processor", fail if we see an error
      waitForHalt s
        | "Error:" `isPrefixOf` s = Stop (Error ("Found error in OpenOCD output: " <> s))
        | "Halting processor" `isPrefixOf` s = Stop Ok
        | otherwise = Continue

    withCreateProcess openOcdProc $ \_ _ (fromJust -> openOcdStdErr) _ -> do
      hSetBuffering openOcdStdErr LineBuffering
      expectLine openOcdStdErr waitForHalt

      withCreateProcess picocomProc $ \maybePicocomStdIn maybePicocomStdOut maybePicocomStdErr _ -> do
        let
          picocomStdInHandle = fromJust maybePicocomStdIn
          picocomStdOutHandle = fromJust maybePicocomStdOut

        hSetBuffering picocomStdInHandle LineBuffering
        hSetBuffering picocomStdOutHandle LineBuffering

        waitForLine picocomStdOutHandle "Terminal ready"

        withCreateProcess gdbProc $ \_ (fromJust -> gdbStdOut) _ _ -> do
          -- Wait for GDB to program the FPGA. If successful, we should see
          -- "going in echo mode" in the picocom output.
          hSetBuffering gdbStdOut LineBuffering
          waitForLine picocomStdOutHandle "listening"

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
              readRemaining h >>= putStrLn

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
