-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedRecordDot #-}

module Bittide.Instances.Hitl.Driver.VexRiscvTcp where

import Prelude

import Bittide.Hitl
import Bittide.Instances.Hitl.Utils.Program

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (SomeException (..), catch)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class
import Data.Time
import Data.Time.Clock.POSIX
import Project.FilePath
import Project.Handle
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import Test.Tasty.HUnit

import Vivado.Tcl
import Vivado.VivadoM

import qualified Bittide.Instances.Hitl.Utils.Driver as D
import qualified Bittide.Instances.Hitl.Utils.Gdb as Gdb
import qualified Bittide.Instances.Hitl.Utils.OpenOcd as Ocd
import qualified Bittide.Instances.Hitl.Utils.Picocom as Picocom
import qualified Data.ByteString.Lazy as BS
import qualified Network.Simple.TCP as NS
import qualified Streaming.ByteString as SBS

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
driverFunc _name [d@(_, dI)] = do
  projectDir <- liftIO $ findParentContaining "cabal.project"

  let
    hitlDir = projectDir </> "_build" </> "hitl"
    mkLogPath str = (hitlDir </> str <> ".log")
    picoOutLog = mkLogPath "picocom-stdout"
    picoErrLog = mkLogPath "picocom-stderr"
    ocdOutLog = mkLogPath "openocd-stdout"
    ocdErrLog = mkLogPath "openocd-stderr"
    gdbOutLog = mkLogPath "gdb-out.log"
    openocdEnv = [("OPENOCD_STDOUT_LOG", ocdOutLog), ("OPENOCD_STDERR_LOG", ocdErrLog)]

  D.assertProbe "probe_test_start" d

  Ocd.withOpenOcdWithEnv openocdEnv dI.usbAdapterLocation 3333 6666 4444 $ \ocd -> do
    liftIO $ do
      hSetBuffering ocd.stderrHandle LineBuffering
      putStr "Waiting for OpenOCD to halt..."
      expectLine ocd.stderrHandle Ocd.waitForHalt
      putStrLn "  Done"

      putStrLn "Starting Picocom..."
    Picocom.withPicocomWithLogging dI.serial picoOutLog picoErrLog $ \pico -> do
      liftIO $ do
        hSetBuffering pico.stdinHandle LineBuffering
        hSetBuffering pico.stdinHandle LineBuffering
        putStrLn "Waiting for Picocom to be ready..."
        D.tryWithTimeout "Waiting for \"Terminal ready\"" 10_000_000 $
          waitForLine pico.stdoutHandle "Terminal ready"

      liftIO $ putStrLn "Starting GDB..."
      Gdb.withGdb $ \gdb -> do
        liftIO $ do
          hSetBuffering gdb.stdinHandle LineBuffering
          Gdb.setLogging gdb gdbOutLog
          Gdb.setFile gdb $ firmwareBinariesDir "riscv32imc" Release </> "smoltcp_client"
          Gdb.setTarget gdb 3333
          errorToException =<< Gdb.loadBinary gdb
          -- errorToException =<< Gdb.compareSections gdb
          Gdb.setBreakpoints
            gdb
            [ "core::panicking::panic"
            , "ExceptionHandler"
            , "smoltcp_client::gdb_panic"
            ]
          Gdb.setBreakpointHook gdb
          Gdb.runCommands gdb.stdinHandle ["continue"]
        D.assertProbe "probe_test_start" d

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
            tryWithTimeout n t io =
              catch (D.tryWithTimeout n t io) $
                \(err :: SomeException) -> loggingSequence >> throwM err

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
