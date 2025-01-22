-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

-- {-# OPTIONS -fplugin-opt=Protocols.Plugin:debug #-}

module Bittide.Instances.Hitl.Driver.VexRiscv where

import Clash.Prelude

import Project.FilePath
import Project.Handle

import Vivado.Tcl (HwTarget)
import Vivado.VivadoM

import Bittide.Hitl
import Bittide.Instances.Hitl.Utils.Gdb
import Bittide.Instances.Hitl.Utils.Program

import Control.Concurrent (threadDelay)
import Control.Monad (forM)
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.List as L
import System.Exit
import System.FilePath
import System.IO
import System.Timeout (timeout)

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
    catchError d ex = do
      putStrLn $ "Test Failed on device " <> d.deviceId <> " with: " <> displayException ex
      pure $ ExitFailure 2

  exitCodes <- forM (L.zip [0 ..] targets) $ \(targetIndex, (hwT, d)) -> handle (liftIO . catchError d) $ do
    liftIO $ putStrLn $ "Running driver for " <> d.deviceId

    openHardwareTarget hwT

    -- even though this is just pre-process step, the CPU is reset until
    -- the test_start signal is asserted and cannot be accessed via GDB otherwise
    updateVio "vioHitlt" [("probe_test_start", "1")]

    let gdbPort = 3333 + targetIndex
    liftIO $ withOpenOcd d.usbAdapterLocation gdbPort 6666 4444 $ \ocd -> do
      -- make sure OpenOCD is started properly

      hSetBuffering ocd.stderrHandle LineBuffering
      expectLine ocd.stderrHandle openOcdWaitForHalt

      -- make sure PicoCom is started properly

      projectDir <- findParentContaining "cabal.project"
      let
        hitlDir = projectDir </> "_build" </> "hitl"
        stdoutLog = hitlDir </> "picocom-stdout." <> show targetIndex <> ".log"
        stderrLog = hitlDir </> "picocom-stderr." <> show targetIndex <> ".log"
      putStrLn $ "logging stdout to `" <> stdoutLog <> "`"
      putStrLn $ "logging stderr to `" <> stderrLog <> "`"

      putStrLn "Starting Picocom..."
      withPicocomWithLogging d.serial stdoutLog stderrLog $ \pico -> do
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
                error $ "Timeout while performing action: " <> actionName
              Just r -> pure r

        tryWithTimeout "Waiting for \"Terminal ready\"" 10_000_000
          $ waitForLine pico.stdoutHandle "Terminal ready"

        -- program the FPGA
        withGdb $ \gdb -> do
          hSetBuffering gdb.stdinHandle LineBuffering

          runGdbCommands
            gdb.stdinHandle
            [ "set logging file ./_build/hitl/gdb-out-" <> show targetIndex <> ".log"
            , "set logging overwrite on"
            , "set logging enabled on"
            , "file \"./_build/cargo/firmware-binaries/riscv32imc-unknown-none-elf/debug/hello\""
            , "target extended-remote :" <> show gdbPort
            , "load"
            ]

          tryWithTimeout "Waiting for \"load done\"" 120_000_000
            $ expectLine gdb.stdoutHandle gdbWaitForLoad

          -- break test
          do
            putStrLn "Testing whether breakpoints work"

            runGdbCommands
              gdb.stdinHandle
              [ "break hello::test_success"
              , "jump _start"
              , gdbEcho "breakpoint reached"
              ]

            tryWithTimeout "Waiting for \"breakpoint reached\"" 10_000_000
              $ waitForLine gdb.stdoutHandle "breakpoint reached"

            runGdbCommands
              gdb.stdinHandle
              [ "disable 1"
              , gdbEcho "continuing"
              , "continue"
              ]

            tryWithTimeout "Waiting for \"continuing\"" 10_000_000
              $ waitForLine gdb.stdoutHandle "continuing"

          -- This is the last thing that will print when the FPGA has been programmed
          -- and starts entereing UART-echo mode.
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
