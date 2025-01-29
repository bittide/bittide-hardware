-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Tests.PanicBacktraces where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)

import Bittide.DoubleBufferedRam (InitialContent (..))
import Bittide.Instances.Hitl.Utils.Gdb
import Bittide.Instances.Hitl.Utils.Program (
  ProcessStdIoHandles (..),
  gdbWaitForLoad,
  gdbWaitForQuit,
  openOcdWaitForHalt,
  withGdb,
 )
import Bittide.ProcessingElement
import Bittide.Wishbone (uartInterfaceWb, uartSim)
import Control.Exception (bracket)
import Data.Char (chr)
import Data.Maybe (fromJust, mapMaybe)
import GHC.Conc (forkIO)
import Network.Socket (PortNumber)
import Project.FilePath
import Project.Handle
import Protocols
import Protocols.Idle
import System.FilePath ((</>))
import System.IO
import System.Posix.Env (getEnvironment)
import System.Posix.Unistd (usleep)
import System.Process
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit
import VexRiscv
import VexRiscv.JtagTcpBridge

import qualified Data.List as List
import qualified Protocols.Df as Df

tests :: TestTree
tests =
  testGroup
    "PanicBacktraces"
    [ testCase
        "test with simple binary"
        $ checkPanicsOn "simple" Release
    , testCase
        "test with intermediate binary"
        $ checkPanicsOn "intermediate" Release
    , testCase
        "test with complex binary"
        $ checkPanicsOn "complex" Release
    ]

vexrJtagBridgeC :: PortNumber -> Circuit () (Jtag dom)
vexrJtagBridgeC portNumber = Circuit go
 where
  go ::
    (Fwd (), Bwd (Jtag dom)) ->
    (Bwd (), Fwd (Jtag dom))
  go (_, jtagOut) = ((), vexrJtagBridge portNumber jtagOut)

dut :: PortNumber -> Circuit () (Df System (BitVector 8))
dut portNumber = withClockResetEnable
  clockGen
  resetGen
  enableGen
  $ circuit
  $ \_unit -> do
    (uartRx) <- idleSource -< ()
    jtagBridge <- vexrJtagBridgeC portNumber -< ()
    [uartBus] <- processingElement NoDumpVcd peConfig -< jtagBridge
    (uartTx, _uartStatus) <- uartInterfaceWb d16 d2 uartSim -< (uartBus, uartRx)
    idC -< uartTx
 where
  peConfig =
    PeConfig
      { memMapConfig = 0b10 :> 0b01 :> 0b11 :> Nil
      , initI = (Undefined @(Div (64 * 1024) 4))
      , initD = (Undefined @(Div (64 * 1024) 4))
      , iBusTimeout = d0
      , dBusTimeout = d0
      }

checkPanicsOn :: String -> CargoBuildType -> Assertion
checkPanicsOn binName buildType = do
  projectDir <- findParentContaining "cabal.project"

  let
    tryWithTimeout :: String -> Int -> IO a -> IO a
    tryWithTimeout actionName dur action = do
      result <- timeout dur action
      case result of
        Nothing -> do
          error $ "Timeout while performing action: " <> actionName
        Just r -> pure r

    openOcdStartPath =
      projectDir
        </> "bittide-instances"
        </> "data"
        </> "openocd"
        </> "start_sim.sh"

    startOpenOcd = do
      currentEnv <- getEnvironment
      let
        openOcdProc =
          (proc openOcdStartPath [])
            { std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            , env =
                Just
                  ( currentEnv
                      <> [ ("OPENOCD_STDOUT_LOG", "_build/hitl/openocd-stdout.log")
                         , ("OPENOCD_STDERR_LOG", "_build/hitl/openocd-stderr.log")
                         ]
                  )
            }
      ocdHandles@(openOcdStdin, openOcdStdout, openOcdStderr, _) <-
        createProcess openOcdProc
      let
        ocdHandles' =
          ProcessStdIoHandles
            { stdinHandle = fromJust openOcdStdin
            , stdoutHandle = fromJust openOcdStdout
            , stderrHandle = fromJust openOcdStderr
            }
      pure (ocdHandles', cleanupProcess ocdHandles)

  _ <- forkIO $ do
    let
      simDut = dut 7894
      cpuOutput =
        List.takeWhile (\c -> c /= '\x04')
          $ fmap (chr . fromIntegral)
          . mapMaybe Df.dataToMaybe
          $ sampleC def simDut
    result <- timeout 120_000_000 (return cpuOutput)
    case result of
      Just output -> putStrLn output
      Nothing -> assertFailure "Getting CPU output timed out!"

  usleep 5_000_000

  bracket startOpenOcd snd $ \(ocd, _) -> do
    hSetBuffering ocd.stderrHandle LineBuffering
    tryWithTimeout "Waiting for OpenOCD to start" 15_000_000
      $ expectLine ocd.stderrHandle openOcdWaitForHalt

    -- Used for manually debugging. If manually debugging, also make sure to comment out
    -- lines 164 through 198. You can then connect to the simulated CPU by following the
    -- GDB commands shown below (logging commands and hook-stop def optional).
    -- usleep 600_000_000

    usleep 5_000_000
    withGdb $ \gdb -> do
      hSetBuffering gdb.stdinHandle LineBuffering

      let
        binDir = firmwareBinariesDir "riscv32imc-unknown-none-elf" buildType
        binFile = binDir </> binName
      runGdbCommands
        gdb.stdinHandle
        [ "set logging file ./_build/hitl/bittide-unittests/" <> binName <> ".log"
        , "set logging overwrite on"
        , "set logging enabled on"
        , "set remotetimeout unlimited"
        , "set debug-file-directory " <> binDir <> "/deps/*:" <> binDir <> "/build/*"
        , "file \"" <> binFile <> "\""
        , "target extended-remote :3333"
        , "load"
        ]

      tryWithTimeout "Waiting for program load to finish" 120_000_000
        $ expectLine gdb.stdoutHandle gdbWaitForLoad

      runGdbCommands
        gdb.stdinHandle
        [ "break " <> binName <> "::gdb_panic"
        , "define hook-stop"
        , "printf \"!!! program stopped executing !!!\\n\""
        , "backtrace"
        , "quit 1"
        , "end"
        , "continue"
        ]

      tryWithTimeout "Waiting for program to hit panic breakpoint" 120_000_000
        $ expectLine gdb.stdoutHandle gdbWaitForQuit
