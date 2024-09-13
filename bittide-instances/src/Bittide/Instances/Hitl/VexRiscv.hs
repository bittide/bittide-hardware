-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

-- {-# OPTIONS -fplugin-opt=Protocols.Plugin:debug #-}

module Bittide.Instances.Hitl.VexRiscv where

import Clash.Annotations.TH (makeTopEntity)

import Clash.Explicit.Prelude (noReset, orReset)
import Clash.Prelude

import Clash.Cores.UART (ValidBaud)
import Clash.Xilinx.ClockGen (clockWizardDifferential)
import Protocols
import Protocols.Wishbone
import VexRiscv

import Bittide.DoubleBufferedRam
import Bittide.Hitl
import Bittide.Instances.Domains (Basic125, Ext125)
import Bittide.ProcessingElement
import Bittide.SharedTypes
import Bittide.Wishbone

import System.Exit (ExitCode (..))

import Paths_bittide_instances

import Control.Monad (unless)
import Control.Monad.Extra (forM_)
import Data.List.Extra (isPrefixOf, trim)
import Data.Maybe (fromJust)
import System.IO
import System.IO.Temp
import System.Process

import Test.Tasty.HUnit

data Error = Ok | Error String
data Filter = Continue | Stop Error

data TestStatus = Running | Success | Fail
  deriving (Enum, Eq, Generic, NFDataX, BitPack)

type TestDone = Bool
type TestSuccess = Bool
type UartRx = Bit
type UartTx = Bit

vexRiscvInner ::
  forall dom.
  ( HiddenClockResetEnable dom
  , 1 <= DomainPeriod dom
  , ValidBaud dom 921600
  ) =>
  Signal dom JtagIn ->
  Signal dom UartRx ->
  ( Signal dom (TestDone, TestSuccess)
  , Signal dom JtagOut
  , Signal dom UartTx
  )
vexRiscvInner jtagIn0 uartRx =
  ( stateToDoneSuccess <$> status
  , jtagOut
  , uartTx
  )
 where
  stateToDoneSuccess Running = (False, False)
  stateToDoneSuccess Success = (True, True)
  stateToDoneSuccess Fail = (True, False)

  ((_, jtagOut), (status, uartTx)) =
    circuitFn ((uartRx, jtagIn0), (pure (), pure ()))

  Circuit circuitFn = circuit $ \(uartRx, jtag) -> do
    [timeBus, uartBus, statusRegisterBus] <- processingElement peConfig -< jtag
    (uartTx, _uartStatus) <- uartWb @dom d16 d16 (SNat @921600) -< (uartBus, uartRx)
    timeWb -< timeBus
    testResult <- statusRegister -< statusRegisterBus
    idC -< (testResult, uartTx)

  statusRegister :: Circuit (Wishbone dom 'Standard 29 (Bytes 4)) (CSignal dom TestStatus)
  statusRegister = Circuit $ \(fwd, _) ->
    let (unbundle -> (m2s, st)) = mealy go Running fwd
     in (m2s, st)
   where
    go st WishboneM2S{..}
      -- out of cycle, no response, same state
      | not (busCycle && strobe) = (st, (emptyWishboneS2M, st))
      -- already done, ACK and same state
      | st /= Running = (st, (emptyWishboneS2M{acknowledge = True}, st))
      -- read, this is write-only, so error, same state
      | not writeEnable =
          ( st
          ,
            ( (emptyWishboneS2M @(Bytes 4))
                { err = True
                , readData = errorX "status register is write-only"
                }
            , st
            )
          )
      -- write! change state, ACK
      | otherwise =
          let state = case writeData of
                1 -> Success
                _ -> Fail
           in (state, (emptyWishboneS2M{acknowledge = True}, state))

  -- ╭────────┬───────┬───────┬────────────────────────────────────╮
  -- │ bin    │ hex   │ bus   │ description                        │
  -- ├────────┼───────┼───────┼────────────────────────────────────┤
  -- │ 0b000. │ 0x0   │       │                                    │
  -- │ 0b001. │ 0x2   │       │                                    │
  -- │ 0b010. │ 0x4   │ 1     │ Data memory                        │
  -- │ 0b011. │ 0x6   │       │                                    │
  -- │ 0b100. │ 0x8   │ 0     │ Instruction memory                 │
  -- │ 0b101. │ 0xA   │ 2     │ Time                               │
  -- │ 0b110. │ 0xC   │ 3     │ UART                               │
  -- │ 0b111. │ 0xE   │ 4     │ Test status register               │
  -- ╰────────┴───────┴───────┴────────────────────────────────────╯
  --
  -- peConfig :: PeConfig 5
  peConfig =
    PeConfig
      (0b100 :> 0b010 :> 0b101 :> 0b110 :> 0b111 :> Nil)
      (Undefined @(Div (64 * 1024) 4)) -- 64 KiB
      (Undefined @(Div (64 * 1024) 4)) -- 64 KiB

vexRiscvTest ::
  "CLK_125MHZ" ::: DiffClock Ext125 ->
  "JTAG" ::: Signal Basic125 JtagIn ->
  "USB_UART_TXD" ::: Signal Basic125 UartRx ->
  ""
    ::: ( "done" ::: Signal Basic125 TestDone
        , "success" ::: Signal Basic125 TestSuccess
        , "JTAG" ::: Signal Basic125 JtagOut
        , "USB_UART_RXD" ::: Signal Basic125 UartTx
        )
vexRiscvTest diffClk jtagIn uartRx = (testDone, testSuccess, jtagOut, uartTx)
 where
  (clk, clkStableRst) = clockWizardDifferential diffClk noReset

  (_, jtagOut, uartTx) =
    withClockResetEnable clk reset enableGen (vexRiscvInner @Basic125 jtagIn uartRx)

  reset = orReset clkStableRst (unsafeFromActiveLow testStarted)

  testStarted :: Signal Basic125 Bool
  testStarted = hitlVioBool clk testDone testSuccess

  -- TODO: We used to perform a HITL test where the CPU would write to a success
  --       register (or a failure register when it would get trapped). We
  --       currently load programs over JTAG instead of preloading them in the
  --       bitstream, making this impossible to do. We should add a _pre_
  --       processing step to the HITL infrastructure, restoring the ability to
  --       do this once more.
  testDone = testStarted
  testSuccess = testStarted
{-# NOINLINE vexRiscvTest #-}
makeTopEntity 'vexRiscvTest

tests :: HitlTestGroup
tests =
  HitlTestGroup
    { topEntity = 'vexRiscvTest
    , extraXdcFiles = ["jtag_config.xdc", "jtag_pmod1.xdc"]
    , externalHdl = []
    , testCases =
        [ HitlTestCase
            { name = "VexRiscV"
            , parameters = paramForSingleHwTarget (HwTargetByIndex 7) ()
            , postProcData = ()
            }
        ]
    , mPostProc = Just postProcessFunc
    }

postProcessFunc :: FilePath -> ExitCode -> IO ()
postProcessFunc _ilaPath _code = case_testGdbProgram
 where
  getOpenOcdStartPath :: IO FilePath
  getOpenOcdStartPath = getDataFileName "data/openocd/start.sh"

  getPicocomStartPath :: IO FilePath
  getPicocomStartPath = getDataFileName "data/picocom/start.sh"

  getGdbProgPath :: IO FilePath
  getGdbProgPath = getDataFileName "data/gdb/test-gdb-prog"

  -- \| XXX: Currently hardcoded to a very specific position. Maybe we could probe
  --          using JTAG to see what device we're connected to?
  --
  getUartDev :: IO String
  getUartDev = pure "/dev/serial/by-path/pci-0000:00:14.0-usb-0:5.1:1.1-port0"

  -- \| Copy the GDB program obtained from 'getGdbProgPath' to a temporary file,
  --    prepend each non-comment, non-empty line with 'echo > {line}\n'. This effectively
  --    emulates Bash's 'set -x' for the GDB program. This can in turn be used to
  --    wait for specific commands to be executed, or simply for debugging.
  --
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
            ( hPutStr dstHandle "echo > "
                >> hPutStr dstHandle line
                >> hPutStrLn dstHandle "\\n"
            )
          hPutStrLn dstHandle line

      hClose dstHandle
      action dstPath

  -- \| Utility function that reads lines from a handle, and applies a filter to
  --    each line. If the filter returns 'Continue', the function will continue
  --    reading lines. If the filter returns @Stop Ok@, the function will return
  --    successfully. If the filter returns @Stop (Error msg)@, the function will
  --    fail with the given message.
  --
  expectLine :: (HasCallStack) => Handle -> (String -> Filter) -> IO ()
  expectLine h f = do
    line <- trim <$> hGetLine h
    let cont = expectLine h f
    if null line
      then cont
      else case f line of
        Continue -> cont
        Stop Ok -> pure ()
        Stop (Error msg) -> assertFailure msg

  -- \| Utility function that reads lines from a handle, and waits for a specific
  --    line to appear. Though this function does not fail in the traditional sense,
  --    it will get stuck if the expected line does not appear. Only use in combination
  --    with sensible time outs (also see 'main').
  --
  waitForLine :: Handle -> String -> IO ()
  waitForLine h expected =
    expectLine h $ \s ->
      if s == expected
        then Stop Ok
        else Continue

  -- \| Test that the GDB program works as expected. This test will start OpenOCD,
  --    Picocom, and GDB, and will wait for the GDB program to execute specific
  --    commands. This test will fail if any of the processes fail, or if the GDB
  --    program does not execute the expected commands.
  --
  --    OpenOCD: A program that communicates with the FPGA over JTAG. When it starts
  --            it will \"interrogate\" the JTAG chain - making sure it can read our
  --            CPU's ID. After that, it will open a GDB server on port 3333.
  --
  --    Picocom: A program that communicates with the FPGA over UART.
  --
  --    GDB: GNU Debugger. This program will connect to the OpenOCD server and is able
  --        to, amongst other things, load programs, set break points, and step
  --        through code.
  --
  case_testGdbProgram :: Assertion
  case_testGdbProgram = do
    startOpenOcdPath <- getOpenOcdStartPath
    startPicocomPath <- getPicocomStartPath
    uartDev <- getUartDev

    withAnnotatedGdbProgPath $ \gdbProgPath -> do
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
