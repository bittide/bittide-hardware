-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

-- {-# OPTIONS -fplugin-opt=Protocols.Plugin:debug #-}

module Bittide.Instances.Hitl.Driver.VexRiscv where

import Clash.Prelude

import Project.FilePath
import Project.Handle

import Vivado.Tcl (HwTarget)
import Vivado.VivadoM

import Bittide.Hitl
import Bittide.Instances.Hitl.Utils.Program

import Control.Concurrent (threadDelay)
import Control.Monad (forM)
import Control.Monad.Catch
import Control.Monad.IO.Class
import System.Exit
import System.FilePath
import System.IO

import qualified Bittide.Instances.Hitl.Utils.OpenOcd as Ocd
import qualified Bittide.Instances.Hitl.Utils.Picocom as Picocom
import qualified Data.List as L
import qualified Gdb
import qualified System.Timeout.Extra as T

driverFunc ::
  String ->
  [ ( HwTarget
    , DeviceInfo
    )
  ] ->
  VivadoM ExitCode
driverFunc _name targets = do
  liftIO
    $ putStrLn
    $ "Running Driver function for targets "
    <> show ((\(_, info) -> info.deviceId) <$> targets)

  let
    catchError :: DeviceInfo -> SomeException -> IO ExitCode
    catchError deviceInfo ex = do
      putStrLn
        $ "Test Failed on device "
        <> deviceInfo.deviceId
        <> " with: "
        <> displayException ex
      pure $ ExitFailure 2

  projectDir <- liftIO $ findParentContaining "cabal.project"

  exitCodes <- forM (L.zip [0 ..] targets) $ \(targetIndex, (hwT, deviceInfo)) -> handle (liftIO . catchError deviceInfo) $ do
    liftIO $ putStrLn $ "Running driver for " <> deviceInfo.deviceId

    let
      hitlDir = projectDir </> "_build" </> "hitl"
      mkLogPath str = (hitlDir </> str <> show targetIndex <> ".log")
      picoOutLog = mkLogPath "picocom-stdout"
      picoErrLog = mkLogPath "picocom-stderr"
      ocdOutLog = mkLogPath "openocd-stdout"
      ocdErrLog = mkLogPath "openocd-stderr"
      gdbOutLog = mkLogPath "gdb-stdout"
      openocdEnv = [("OPENOCD_STDOUT_LOG", ocdOutLog), ("OPENOCD_STDERR_LOG", ocdErrLog)]
      gdbPort = 3333 + targetIndex

    openHardwareTarget hwT

    -- even though this is just pre-process step, the CPU is reset until
    -- the test_start signal is asserted and cannot be accessed via GDB otherwise
    updateVio "vioHitlt" [("probe_test_start", "1")]
    liftIO $ Ocd.withOpenOcdWithEnv openocdEnv deviceInfo.usbAdapterLocation gdbPort 6666 4444 $ \ocd -> do
      -- make sure OpenOCD is started properly
      hSetBuffering ocd.stderrHandle LineBuffering
      expectLine_ ocd.stderrHandle Ocd.waitForInitComplete

      putStrLn "Starting Picocom..."
      putStrLn $ "Logging output to '" <> hitlDir
      Picocom.withPicocomWithLogging
        Picocom.defaultStdStreams
        deviceInfo.serial
        Picocom.parameters{Picocom.stdOut = picoOutLog, Picocom.stdErr = picoErrLog}
        $ \pico -> do
          hSetBuffering pico.stdinHandle LineBuffering
          hSetBuffering pico.stdoutHandle LineBuffering

          let
            -- Create function to log the output of the processes
            loggingSequence :: IO ()
            loggingSequence = do
              threadDelay 1_000_000 -- Wait 1 second for data loggers to catch up
              putStrLn "Picocom stdout"
              picocomOut <- readRemainingChars pico.stdoutHandle
              putStrLn picocomOut
              putStrLn "Picocom StdErr"
              readRemainingChars pico.stderrHandle >>= putStrLn

            tryWithTimeout :: String -> Int -> IO a -> IO a
            tryWithTimeout n t io =
              catch (T.tryWithTimeout T.PrintActionTime n t io)
                $ \(err :: SomeException) -> loggingSequence >> throwM err

          tryWithTimeout "Waiting for \"Terminal ready\"" 10_000_000
            $ waitForLine pico.stdoutHandle "Terminal ready"

          -- program the FPGA
          Gdb.withGdb $ \gdb -> do
            Gdb.setLogging gdb gdbOutLog
            Gdb.setFile gdb $ firmwareBinariesDir "riscv32imc" Debug </> "vexriscv-hello"
            Gdb.setTarget gdb gdbPort
            assertEither =<< Gdb.loadBinary gdb

            -- break test
            do
              putStrLn "Testing whether breakpoints work"
              -- the hyphen in the binary names becomes an underscore because reasons
              Gdb.setBreakpoints gdb ["vexriscv_hello::test_success"]
              Gdb.continue gdb
              Gdb.echo gdb "breakpoint reached"
              Gdb.runCommand gdb "disable 1"
              Gdb.continue gdb

            -- This is the last thing that will print when the FPGA has been programmed
            -- and starts entering UART-echo mode.
            tryWithTimeout "Waiting for \"Going in echo mode!\"" 10_000_000
              $ waitForLine pico.stdoutHandle "Going in echo mode!"

            -- Test UART echo
            hPutStrLn pico.stdinHandle "Hello, UART!"
            tryWithTimeout "Waiting for \"Hello, UART!!\"" 10_000_000
              $ waitForLine pico.stdoutHandle "Hello, UART!"

    updateVio "vioHitlt" [("probe_test_start", "0")]

    pure ExitSuccess

  let exitCode =
        L.foldl
          ( \acc code ->
              case code of
                ExitSuccess -> acc
                _ -> code
          )
          ExitSuccess
          exitCodes

  pure exitCode
