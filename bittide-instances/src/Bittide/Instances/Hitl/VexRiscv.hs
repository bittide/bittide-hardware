-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

-- {-# OPTIONS -fplugin-opt=Protocols.Plugin:debug #-}

module Bittide.Instances.Hitl.VexRiscv where

import Clash.Annotations.TH (makeTopEntity)

import Clash.Prelude
import Clash.Explicit.Prelude (noReset, orReset)

import Clash.Cores.UART (ValidBaud)
import Clash.Xilinx.ClockGen (clockWizardDifferential)
import Clash.Hitl (HitlTests, allFpgas, hitlVioBool, noConfigTest)
import Language.Haskell.TH (runIO)
import Protocols
import Protocols.Internal
import Protocols.Wishbone
import System.FilePath
import VexRiscv

import Bittide.DoubleBufferedRam
import Bittide.Instances.Domains (Basic200, Ext125)
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util (memBlobsFromElf)
import Bittide.SharedTypes
import Bittide.Wishbone
import Project.FilePath

data TestStatus = Running | Success | Fail
  deriving (Enum, Eq, Generic, NFDataX, BitPack)

type TestDone = Bool
type TestSuccess = Bool
type UartRx = Bit
type UartTx = Bit

vexRiscvInner ::
  forall dom .
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
  , jtagOut1
  , uartTx
  )
  where
    stateToDoneSuccess Running = (False, False)
    stateToDoneSuccess Success = (True, True)
    stateToDoneSuccess Fail    = (True, False)

    unitC = CSignal (pure ())
    (_, (CSignal status, CSignal uartTx, CSignal jtagOut1)) =
      circuitFn ((CSignal uartRx, CSignal jtagIn0), (unitC, unitC, unitC))

    Circuit circuitFn = circuit $ \(uartRx, jtagIn1) -> do
        ([timeBus, uartBus, statusRegisterBus], jtagOut0) <- processingElement peConfig -< jtagIn1
        (uartTx, _uartStatus) <- uartWb @dom d16 d16 (SNat @921600) -< (uartBus, uartRx)
        timeWb -< timeBus
        testResult <- statusRegister -< statusRegisterBus
        idC -< (testResult, uartTx, jtagOut0)

    statusRegister :: Circuit (Wishbone dom 'Standard 29 (Bytes 4)) (CSignal dom TestStatus)
    statusRegister = Circuit $ \(fwd, CSignal _) ->
        let (unbundle -> (m2s, st)) = mealy go Running fwd
        in (m2s, CSignal st)
      where
        go st WishboneM2S{..}
          -- out of cycle, no response, same state
          | not (busCycle && strobe) = (st, (emptyWishboneS2M, st))
          -- already done, ACK and same state
          | st /= Running = (st, (emptyWishboneS2M { acknowledge = True}, st))
          -- read, this is write-only, so error, same state
          | not writeEnable =
              ( st
              , ((emptyWishboneS2M @(Bytes 4))
                  { err = True
                  , readData = errorX "status register is write-only"
                  }
                , st))
          -- write! change state, ACK
          | otherwise =
              let state = case writeData of
                    1 -> Success
                    _ -> Fail
              in (state, (emptyWishboneS2M { acknowledge = True }, state))

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
    peConfig = PeConfig
      (0b100 :> 0b010 :> 0b101 :> 0b110 :> 0b111 :> Nil)
      -- (0b100 :> 0b010 :> 0b101 :> 0b110 :> Nil)
      -- these memories need to be reloadable, otherwise synthesis will optimise
      -- out the entire data memory somehow!
      (Reloadable $ Blob iMem)
      (Reloadable $ Blob dMem)

    (   (_iStart, _iSize, iMem)
      , (_dStart, _dSize, dMem)) = $(do

        root <- runIO $ findParentContaining "cabal.project"
        let
          elfDir = root </> firmwareBinariesDir "riscv32imc-unknown-none-elf" False
          elfPath = elfDir </> "hello"

        memBlobsFromElf BigEndian elfPath Nothing)


vexRiscvTest ::
  "CLK_125MHZ" ::: DiffClock Ext125 ->
  "JTAG" ::: Signal Basic200 JtagIn ->
  "USB_UART_TXD" ::: Signal Basic200 UartRx ->
  "" :::
    ( "done"    ::: Signal Basic200 TestDone
    , "success" ::: Signal Basic200 TestSuccess
    , "JTAG"    ::: Signal Basic200 JtagOut
    , "USB_UART_RXD" ::: Signal Basic200 UartTx
    )
vexRiscvTest diffClk jtagIn uartRx = (testDone, testSuccess, jtagOut, uartTx)
  where
    (clk, clkStableRst) = clockWizardDifferential diffClk noReset

    (unbundle -> (testDone, testSuccess), jtagOut, uartTx) =
      withClockResetEnable clk reset enableGen (vexRiscvInner @Basic200 jtagIn uartRx)

    reset = orReset clkStableRst (unsafeFromActiveLow testStarted)

    testStarted :: Signal Basic200 Bool
    testStarted = hitlVioBool clk testDone testSuccess

{-# NOINLINE vexRiscvTest #-}
makeTopEntity 'vexRiscvTest

tests :: HitlTests ()
tests = noConfigTest "VexRiscV" allFpgas
