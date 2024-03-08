-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Bittide.Instances.Hitl.VexRiscv where

import Clash.Annotations.TH (makeTopEntity)

import Clash.Prelude
import Clash.Explicit.Prelude (noReset, orReset)

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
import Project.FilePath

data TestStatus = Running | Success | Fail
  deriving (Enum, Eq, Generic, NFDataX, BitPack)

vexRiscvInner ::
  forall dom.
  HiddenClockResetEnable dom =>
  Signal dom JtagIn ->
  Signal dom (Bool, Bool, JtagOut)
vexRiscvInner jtagIn0 = bundle (done, success, jtagOut1)
  where

    (unbundle -> (done, success)) = stateToDoneSuccess <$> status

    stateToDoneSuccess Running = (False, False)
    stateToDoneSuccess Success = (True, True)
    stateToDoneSuccess Fail    = (True, False)

    unitC = CSignal (pure ())
    (_, (CSignal status, CSignal jtagOut1)) = circuitFn (CSignal jtagIn0, (unitC, unitC))

    Circuit circuitFn = circuit $ \jtagIn1 -> do
        ([wb], jtagOut0) <- processingElement peConfig -< jtagIn1
        testResult <- statusRegister -< wb
        idC -< (testResult, jtagOut0)

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
    bin        hex  bus  description
    0b00xxxxxx 0x0. 0    NOT USED
    0b11xxxxxx 0xC. 1    instruction memory
    0b01xxxxxx 0x4. 2    data memory
    0b10xxxxxx 0x8. 3    status register
    -}

    peConfig = PeConfig
      (0b11 :> 0b01 :> 0b10 :> Nil)
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
  "JTAG" ::: Signal Basic200 JtagIn ->
  "" :::
    ( "done"    ::: Signal Basic200 Bool
    , "success" ::: Signal Basic200 Bool
    , "JTAG"    ::: Signal Basic200 JtagOut
    )
vexRiscvTest diffClk jtagIn = (testDone, testSuccess, jtagOut)
  where
    (clk, clkStableRst) = clockWizardDifferential diffClk noReset

    (testDone, testSuccess) = unbundle $
      withClockResetEnable clk reset enableGen (vexRiscvInner @Basic200)

    reset = orReset clkStableRst (unsafeFromActiveLow testStarted)

    testStarted :: Signal Basic200 Bool
    testStarted = hitlVioBool clk testDone testSuccess

{-# NOINLINE vexRiscvTest #-}
makeTopEntity 'vexRiscvTest

tests :: HitlTests ()
tests = noConfigTest "VexRiscV" allFpgas
