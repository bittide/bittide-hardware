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
import Bittide.Instances.Hitl.Utils.Vivado

import Control.Concurrent
import Control.Concurrent.Async
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

import Vivado
import Vivado.Tcl

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

preProcessFunc ::
  VivadoHandle ->
  String ->
  FilePath ->
  HwTarget ->
  DeviceInfo ->
  IO
    ( TestStepResult
        ( ProcessStdIoHandles
        , ProcessStdIoHandles
        , ProcessStdIoHandles
        , IO ()
        )
    )
preProcessFunc v _name ilaPath hwT deviceInfo = do
  openHwTarget v hwT
  execCmd_ v "set_property" ["PROBES.FILE", embrace ilaPath, "[current_hw_device]"]
  refresh_hw_device v []

  execCmd_ v "set_property" ["OUTPUT_VALUE", "1", getProbeTestStartTcl]
  commit_hw_vio v ["[get_hw_vios]"]

  (ocd, ocdClean) <- startOpenOcd deviceInfo.usbAdapterLocation 3333 6666 4444

  hSetBuffering ocd.stderrHandle LineBuffering

  putStr "Waiting for halt..."
  expectLine ocd.stderrHandle openOcdWaitForHalt
  putStrLn " Done"

  -- make sure PicoCom is started properly

  projectDir <- findParentContaining "cabal.project"
  let
    hitlDir = projectDir </> "_build" </> "hitl"
    stdoutLog = hitlDir </> "picocom-stdout.log"
    stderrLog = hitlDir </> "picocom-stderr.log"
  putStrLn $ "logging stdout to `" <> stdoutLog <> "`"
  putStrLn $ "logging stderr to `" <> stderrLog <> "`"

  putStrLn "Starting Picocom..."
  (pico, picoClean) <-
    startPicocomWithLogging
      deviceInfo.serial
      stdoutLog
      stderrLog

  hSetBuffering pico.stdinHandle LineBuffering
  hSetBuffering pico.stdoutHandle LineBuffering

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

  putStrLn "Waiting for Picocom to be ready..."
  tryWithTimeout "Picocom handshake" 10_000_000 $
    waitForLine pico.stdoutHandle "Terminal ready"

  -- program the FPGA
  putStrLn "Starting GDB..."
  (gdb, gdbClean) <- startGdb

  hSetBuffering gdb.stdinHandle LineBuffering

  runGdbCommands
    gdb.stdinHandle
    [ "set logging file ./_build/hitl/gdb-out.log"
    , "set logging overwrite on"
    , "set logging enabled on"
    , "file \"./_build/cargo/firmware-binaries/riscv32imc-unknown-none-elf/release/smoltcp_client\""
    , "target extended-remote :3333"
    , "load"
    , gdbEcho "load done"
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

  tryWithTimeout "Waiting for program to be loaded" 120_000_000 $
    waitForLine gdb.stdoutHandle "load done"

  pure $ TestStepSuccess (ocd, pico, gdb, gdbClean >> picoClean >> ocdClean)

driverFunc ::
  VivadoHandle ->
  String ->
  FilePath ->
  [(HwTarget, DeviceInfo)] ->
  IO ExitCode
driverFunc v _name ilaPath [(hwT, dI)] = do
  preProcessResult <- preProcessFunc v _name ilaPath hwT dI

  (_ocd, pico, _gdb, cleanupFn) <- case preProcessResult of
    TestStepSuccess out -> pure out
    TestStepFailure reason -> assertFailure $ "test failed. reason: " <> reason

  openHwTarget v hwT
  execCmd_ v "set_property" ["PROBES.FILE", embrace ilaPath, "[current_hw_device]"]
  refresh_hw_device v []

  execCmd_ v "set_property" ["OUTPUT_VALUE", "1", getProbeTestStartTcl]
  commit_hw_vio v ["[get_hw_vios]"]

  putStrLn "Starting TCP Server"
  withServer $ \(serverSock, _) -> do
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

  cleanupFn

  pure ExitSuccess
driverFunc _v _name _ilaPath _ = error "Ethernet/VexRiscvTcp driver func should only run with one hardware target"

{- | Test that the `Bittide.Instances.Hitl.Ethernet:vexRiscvTcpTest` design programmed
with `smoltcp_client` can connect to a TCP server and stress test it for a short duration.

This test will start run a TCP server to which the client can connect. Furthermore it uses
OpenOCD, Picocom, and GDB, and will wait for the GDB program to program the soft core.

Then it will wait until the softcore responds and connects to the TCP server. This
test consumes all data produced by the TCP client and measure the average speed.

This test will fail if any of the subprocesses fail, or if the client does not manage to
connect to the server and close the connection within a reasonable time.
-}

{-
case_testTcpClient :: Assertion
case_testTcpClient = do
  let
    -- For now we use a hardcoded device. This should be updated with the pre-processing
    -- infrastructure.
    uartDev = (last demoRigInfo).serial
    adapterLoc = (last demoRigInfo).usbAdapterLocation
  startOpenOcdPath <- getOpenOcdStartPath
  startPicocomPath <- getPicocomStartPath
  gdbScriptPath <- getGdbScriptPath

  putStrLn "Starting TCP Server"
  withServer $ \(serverSock, _) -> do
    withAnnotatedGdbScriptPath gdbScriptPath $ \gdbProgPath -> do
      currentEnv <- getEnvironment
      projectDir <- findParentContaining "cabal.project"
      let
        hitlDir = projectDir </> "_build" </> "hitl"
        stdoutLog = hitlDir </> "picocom-stdout.log"
        stderrLog = hitlDir </> "picocom-stderr.log"
      putStrLn $ "logging stdout to `" <> stdoutLog <> "`"
      putStrLn $ "logging stderr to `" <> stderrLog <> "`"
      let
        openOcdProc =
          (proc startOpenOcdPath [])
            { env = Just (currentEnv <> [("USB_DEVICE", adapterLoc)])
            , std_err = CreatePipe
            }
        gdbProc =
          (proc "gdb" ["--command", gdbProgPath])
            { std_out = CreatePipe
            , std_err = CreatePipe
            }
        picocomProc =
          (proc startPicocomPath [uartDev])
            { std_out = CreatePipe
            , std_in = CreatePipe
            , new_session = True
            , env =
                Just
                  (currentEnv <> [("PICOCOM_STDOUT_LOG", stdoutLog), ("PICOCOM_STDERR_LOG", stderrLog)])
            }

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
            loggingSequence
-}

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
