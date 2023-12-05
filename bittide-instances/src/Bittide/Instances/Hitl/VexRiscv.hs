-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Bittide.Instances.Hitl.VexRiscv where

import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.Xilinx.VIO (vioProbe)

import Clash.Prelude
import Clash.Explicit.Prelude (noReset, orReset)

import Clash.Xilinx.ClockGen (clockWizardDifferential)
import Language.Haskell.TH (runIO)
import Protocols
import Protocols.Internal
import Protocols.Wishbone
import System.FilePath

import Bittide.DoubleBufferedRam
import Bittide.Instances.Domains (Basic200, Ext125)
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util (memBlobsFromElf)
import Bittide.SharedTypes
import Project.FilePath

data TestStatus = Running | Success | Fail
  deriving (Enum, Eq, Generic, NFDataX, BitPack)

vexRiscvInner ::
  forall dom.
  HiddenClockResetEnable dom =>
  Signal dom (Bool, Bool)
vexRiscvInner = stateToDoneSuccess <$> status
  where

    stateToDoneSuccess Running = (False, False)
    stateToDoneSuccess Success = (True, True)
    stateToDoneSuccess Fail    = (True, False)

    unitC = CSignal (pure ())
    (_, CSignal status) = circuitFn ((), unitC)

    Circuit circuitFn = circuit $ \unit -> do
        [wb] <- processingElement peConfig -< unit
        statusRegister -< wb

    statusRegister :: Circuit (Wishbone dom 'Standard 30 (Bytes 4)) (CSignal dom TestStatus)
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

    {-
    0b1000xxxx 0x8x instruction memory
    0b0100xxxx 0x4x data memory
    0b1100xxxx 0xCx status register
    -}

    peConfig = PeConfig
      (0b10 :> 0b01 :> 0b11 :> Nil)
      -- these memories need to be reloadable, otherwise synthesis will optimise
      -- out the entire data memory somehow!
      (Reloadable $ Blob iMem)
      (Reloadable $ Blob dMem)

    (   (_iStart, _iSize, iMem)
      , (_dStart, _dSize, dMem)) = $(do

        root <- runIO $ findParentContaining "cabal.project"
        let
          elfDir = root </> firmwareBinariesDir "riscv32imc-unknown-none-elf" True
          elfPath = elfDir </> "processing-element-test"

        memBlobsFromElf BigEndian elfPath Nothing)


vexRiscvTest ::
  "CLK_125MHZ" ::: DiffClock Ext125 ->
  "" :::
    ( "done"    ::: Signal Basic200 Bool
    , "success" ::: Signal Basic200 Bool
    )
vexRiscvTest diffClk = (testDone, testSuccess)
  where
    (clk, clkStableRst) = clockWizardDifferential diffClk noReset

    (testDone, testSuccess) = unbundle $
      withClockResetEnable clk reset enableGen (vexRiscvInner @Basic200)

    reset = orReset clkStableRst (unsafeFromActiveLow testReset)

    testReset :: Signal Basic200 Bool
    testReset =
      setName @"vioHitlt" $
      vioProbe
        (  "probe_test_done"
        :> "probe_test_success"
        :> Nil)
        ( "probe_test_start" :> Nil)
        False
        clk
        testDone
        testSuccess

{-# NOINLINE vexRiscvTest #-}
makeTopEntity 'vexRiscvTest
