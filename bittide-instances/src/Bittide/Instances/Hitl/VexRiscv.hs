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
import Bittide.Instances.Hitl.Setup
import Bittide.ProcessingElement
import Bittide.SharedTypes
import Bittide.Wishbone

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

  statusRegister :: Circuit (Wishbone dom 'Standard 27 (Bytes 4)) (CSignal dom TestStatus)
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
            , parameters = paramForHwTargets allHwTargets ()
            , postProcData = ()
            }
        ]
    , mPostProc = Just "post-vex-riscv-test"
    }
