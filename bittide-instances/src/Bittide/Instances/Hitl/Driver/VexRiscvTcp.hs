-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Bittide.Instances.Hitl.Driver.VexRiscvTcp where

import Prelude

import Bittide.Hitl
import Bittide.Instances.Hitl.Utils.Gdb
import Bittide.Instances.Hitl.Utils.Program

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.IO.Class
import Data.List.Extra
import Data.Time
import Data.Time.Clock.POSIX
import Project.FilePath
import Project.Handle
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Timeout
import Test.Tasty.HUnit

import Vivado.Tcl
import Vivado.VivadoM

import qualified Data.ByteString.Lazy as BS
import qualified Network.Simple.TCP as NS
import qualified Streaming.ByteString as SBS

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

withServer :: ((NS.Socket, NS.SockAddr) -> IO a) -> IO a
withServer = NS.listen NS.HostAny "1234"

startServer :: IO (NS.Socket, NS.SockAddr)
startServer = do
  (serverSock, serverAddr) <- NS.bindSock (NS.Host "0.0.0.0") "1234"
  putStrLn $ "Listening for connections on " <> show serverAddr
  NS.listenSock serverSock 2048
  pure (serverSock, serverAddr)

waitForClients :: Int -> NS.Socket -> IO [(NS.Socket, NS.SockAddr)]
waitForClients numberOfClients serverSock = do
  mapM
    ( \i ->
        NS.accept
          serverSock
          ( \(clientSock, clientAddr) -> do
              putStrLn $ show i <> " | Connection established from: " ++ show clientAddr
              pure (clientSock, clientAddr)
          )
    )
    [1 .. numberOfClients]

driverFunc ::
  String ->
  [(HwTarget, DeviceInfo)] ->
  VivadoM ExitCode
driverFunc testName [(hwT, dI)] = do
  projectDir <- liftIO $ findParentContaining "cabal.project"

  let
    hitlDir = projectDir </> "_build" </> "hitl" </> testName
    mkLogPath str = (hitlDir </> str <> ".log")
    picoOutLog = mkLogPath "picocom-stdout"
    picoErrLog = mkLogPath "picocom-stderr"
    ocdOutLog = mkLogPath "openocd-stdout"
    ocdErrLog = mkLogPath "openocd-stderr"
    gdbOutLog = mkLogPath "gdb-out.log"
    openocdEnv = [("OPENOCD_STDOUT_LOG", ocdOutLog), ("OPENOCD_STDERR_LOG", ocdErrLog)]

    initHwDevice = do
      openHardwareTarget hwT
      updateVio "vioHitlt" [("probe_test_start", "1")]

    doWithTimeout :: Int -> IO a -> IO a
    doWithTimeout time action = do
      result <- timeout time action
      case result of
        Nothing -> error "Action failed with timeout."
        Just value -> pure value

    initOpenOcd :: ProcessStdIoHandles -> IO ()
    initOpenOcd ocd = do
      hSetBuffering ocd.stderrHandle LineBuffering
      putStr "Waiting for OpenOCD to halt..."
      expectLine ocd.stderrHandle openOcdWaitForHalt
      putStrLn "  Done"

    initPicocom :: ProcessStdIoHandles -> IO ()
    initPicocom pico = do
      hSetBuffering pico.stdinHandle LineBuffering
      hSetBuffering pico.stdinHandle LineBuffering

      putStrLn "Waiting for Picocom to be ready..."
      doWithTimeout 10_000_000 $ waitForLine pico.stdoutHandle "Terminal ready"

    initGdb :: ProcessStdIoHandles -> IO ()
    initGdb gdb = do
      hSetBuffering gdb.stdinHandle LineBuffering

      runGdbCommands
        gdb.stdinHandle
        [ "set logging file " <> gdbOutLog
        , "set logging overwrite on"
        , "set logging enabled on"
        , "file \"./_build/cargo/firmware-binaries/riscv32imc-unknown-none-elf/release/smoltcp_client\""
        , "target extended-remote :3333"
        , "load"
        , gdbEcho "load done"
        , gdbEcho "Compare sections"
        , "compare-sections"
        , "break core::panicking::panic"
        , "break ExceptionHandler"
        , "break rust_begin_unwind"
        , "break smoltcp_client::gdb_panic"
        , "define hook-stop"
        , "printf '!!! program stopped executing !!!\\n'"
        , "i r"
        , "bt"
        , "quit 1"
        , "end"
        , "continue"
        ]
      doWithTimeout 120_000_000 $ waitForLine gdb.stdoutHandle "load done"

    startTest = do
      openHardwareTarget hwT
      updateVio "vioHitlt" [("probe_test_start", "1")]

  initHwDevice

  withOpenOcdWithEnv openocdEnv dI.usbAdapterLocation 3333 6666 4444 $ \ocd -> do
    liftIO $ initOpenOcd ocd

    liftIO $ putStrLn "Starting Picocom..."
    withPicocomWithLogging dI.serial picoOutLog picoErrLog $ \pico -> do
      liftIO $ initPicocom pico

      liftIO $ putStrLn "Starting GDB..."
      withGdb $ \gdb -> do
        liftIO $ initGdb gdb

        startTest

        liftIO $ putStrLn "Starting TCP server"
        liftIO $ withServer $ \(serverSock, _) -> do
          let
            -- Create function to log the output of the processes
            loggingSequence = do
              threadDelay 1_000_000 -- Wait 1 second for data loggers to catch up
              putStrLn "Picocom stdout"
              picocomOut <- readRemainingChars pico.stdoutHandle
              putStrLn picocomOut

              putStrLn "Picocom StdErr"
              readRemainingChars pico.stderrHandle >>= putStrLn

            tryWithTimeout :: String -> Int -> IO a -> IO a
            tryWithTimeout actionName dur action = do
              result <- timeout dur action
              case result of
                Nothing -> do
                  loggingSequence
                  assertFailure $ "Timeout while performing action: " <> actionName
                Just r -> pure r

          putStrLn "Waiting for \"Starting TCP Client\""

          tryWithTimeout "Handshake softcore" 10_000_000 $
            waitForLine pico.stdoutHandle "Starting TCP Client"

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
          loggingSequence

          return ExitSuccess
driverFunc _name _ = error "Ethernet/VexRiscvTcp driver func should only run with one hardware target"

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
