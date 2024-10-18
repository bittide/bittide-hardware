-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Prelude

import Bittide.Instances.Hitl.Post.TcpServer
import Control.Concurrent
import Control.Concurrent.Async
import Data.List.Extra
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import Paths_bittide_instances
import Project.FilePath
import Project.Handle
import Project.Programs
import System.Directory
import System.Environment (withArgs)
import System.FilePath
import System.IO
import System.Process
import System.Timeout
import Test.Tasty.HUnit
import Test.Tasty.TH

import qualified Data.ByteString.Lazy as BS
import qualified Network.Simple.TCP as NS
import qualified Streaming.ByteString as SBS

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

-- | Directory to dump TCP data sent by clients
tcpDataDir :: FilePath
tcpDataDir = buildDir </> "data" </> "tcp"

{- | Test that the `Bittide.Instances.Hitl.Ethernet:vexRiscvTcpTest` design programmed
with `smoltcp_client` can connect to a TCP server and stress test it for a short duration.

This test will start run a TCP server to which the client can connect. Furthermore it uses
OpenOCD, Picocom, and GDB, and will wait for the GDB program to programm the soft core.

Then it will wait untill the softcore responds and connects to the TCP server. This
test consumes all data produced by the TCP client and measure the average speed.

This test will fail if any of the subprocesses fail, or if the client does not manage to
connect to the server and close the connection within a reasonable time.
-}
case_testTcpClient :: Assertion
case_testTcpClient = do
  startOpenOcdPath <- getOpenOcdStartPath
  startPicocomPath <- getPicocomStartPath
  uartDev <- getUartDev
  gdbScriptPath <- getGdbScriptPath

  putStrLn "Starting TCP Server"
  (serverSock, _) <- startServer
  withAnnotatedGdbScriptPath gdbScriptPath $ \gdbProgPath -> do
    let
      openOcdProc = (proc startOpenOcdPath []){std_err = CreatePipe}
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

          -- Create function to log the output of the processes
          loggingSequence = do
            threadDelay 1_000_000 -- Wait 1 second for data loggers to catch up
            putStrLn "Picocom stdout"
            picocomOut <- readRemainingChars picocomStdOutHandle
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

        hSetBuffering picocomStdInHandle LineBuffering
        hSetBuffering picocomStdOutHandle LineBuffering

        putStrLn "Waiting for Picocom to be ready..."
        tryWithTimeout "Picocom handshake" 10_000_000 $
          waitForLine picocomStdOutHandle "Terminal ready"

        putStrLn "Starting GDB..."
        withCreateProcess gdbProc $ \_ (fromJust -> gdbStdOut) _ _ -> do
          hSetBuffering gdbStdOut LineBuffering

          putStrLn "Waiting for \"Starting TCP Client\""
          tryWithTimeout "Handshake softcore" 10_000_000 $
            waitForLine picocomStdOutHandle "Starting TCP Client"

          let numberOfClients = 1
          putStrLn $ "Waiting for " <> show numberOfClients <> " clients to connect to TCP server."
          clients <-
            tryWithTimeout "Wait for clients" 60_000_000 $
              waitForClients numberOfClients serverSock

          putStrLn "Receiving client data"
          createDirectoryIfMissing True tcpDataDir
          tryWithTimeout "Receive client data" 60_000_000 $
            mapConcurrently_ runTcpTest clients

          putStrLn "Closing client connections"
          tryWithTimeout "Closing connections" 10_000_000 $
            mapConcurrently_ (NS.closeSock . fst) clients
          putStrLn "Closing server connection"
          NS.closeSock serverSock
          loggingSequence

runTcpTest :: (NS.Socket, NS.SockAddr) -> Assertion
runTcpTest (sock, sockAddr) = do
  putStrLn $ "Testing TCP connection from " <> show sockAddr
  start <- getPOSIXTime
  bs <- SBS.toLazy_ $ SBS.reread (const $ NS.recv sock (1024 * 1024)) ()
  stop <- getPOSIXTime
  let
    size = BS.length bs
    diff = nominalDiffTimeToSeconds $ stop - start
    speed = fromIntegral size / diff
  putStrLn $
    show sockAddr
      <> " sent "
      <> show size
      <> " bytes in "
      <> show (round diff :: Integer)
      <> " seconds ("
      <> show (round speed :: Integer)
      <> " bytes/s)"
  assertEqual
    "Expected and actual bytestring are not equal"
    bs
    (BS.replicate (BS.length bs) 0x00)

main :: IO ()
main = withArgs [] $defaultMainGenerator
