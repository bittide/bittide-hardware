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

import Vivado
import Vivado.Tcl

import Bittide.DoubleBufferedRam
import Bittide.Hitl
import Bittide.Instances.Domains (Basic125, Ext125)
import Bittide.Instances.Hitl.Pre.Program
import Bittide.ProcessingElement
import Bittide.SharedTypes
import Bittide.Wishbone

import Paths_bittide_instances

import System.IO
import System.Exit

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
            , preProc = InheritPreProcess
            }
        ]
    , mPreProc = preProcessFunc
    , mMonitorProc = Just monitorFunc
    , mPostProc = Nothing
    }

getTestProbeTcl :: String -> String
getTestProbeTcl probeNm =
  "[get_hw_probes -of_objects [get_hw_vios] " <> probeNm <> "]"

{- | Tcl code to get the HITL VIO test start output probe.
Run `verifyHitlVio` beforehand to ensure that the probe is available.
-}
getProbeTestStartTcl :: String
getProbeTestStartTcl = getTestProbeTcl "*vioHitlt/probe_test_start"

monitorFunc :: VivadoHandle -> String -> FilePath -> [(HwTarget, c)] -> IO ExitCode
monitorFunc v _name ilaPath [(hwT, _preData)] = do
  openHwT v hwT
  execCmd_ v "set_property" ["PROBES.FILE", embrace ilaPath, "[current_hw_device]"]
  refresh_hw_device v []

  gdbScript <- getDataFileName "data/gdb/test-gdb-prog"

  execCmd_ v "set_property" ["OUTPUT_VALUE", "1", getProbeTestStartTcl]
  commit_hw_vio v ["[get_hw_vios]"]

  runGdbPicocomOpenOcd gdbScript $ \gdbOut (picocomIn, picocomOut) -> do
    -- This is the first thing that will print when the FPGA has been programmed
    -- and starts executing the new program.
    waitForLine picocomOut "Going in echo mode!"

    -- Wait for GDB to reach its last command - where it will wait indefinitely
    waitForLine gdbOut "> continue"

    -- Test UART echo
    hPutStrLn picocomIn "Hello, UART!"
    waitForLine picocomOut "Hello, UART!"

  execCmd_ v "set_property" ["OUTPUT_VALUE", "0", getProbeTestStartTcl]
  commit_hw_vio v ["[get_hw_vios]"]


  pure $ ExitSuccess
monitorFunc _v _name _ilaPath _ = error "VexRiscv monitor func should only run with one hardware target"

preProcessFunc :: VivadoHandle -> String -> HwTarget -> IO (TestStepResult ())
preProcessFunc _v _name _hwT = do
  pure $ TestStepSuccess ()

  -- gdbScript <- getDataFileName "data/gdb/test-gdb-prog"

  -- runGdbPicocomOpenOcd gdbScript $ \gdbOut (picocomIn, picocomOut) -> do
  --   -- This is the first thing that will print when the FPGA has been programmed
  --   -- and starts executing the new program.
  --   waitForLine picocomOut "Going in echo mode!"

  --   -- Wait for GDB to reach its last command - where it will wait indefinitely
  --   waitForLine gdbOut "> continue"

  --   -- Test UART echo
  --   hPutStrLn picocomIn "Hello, UART!"
  --   waitForLine picocomOut "Hello, UART!"
  -- pure ()
