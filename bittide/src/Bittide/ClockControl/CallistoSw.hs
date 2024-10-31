-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Bittide.ClockControl.CallistoSw (
  callistoSwClockControl,
  SwControlConfig (..),
) where

import Clash.Explicit.Prelude hiding (PeriodToCycles)
import Clash.Prelude (withClockResetEnable)

import Clash.Cores.Xilinx.Ila (Depth (..), IlaConfig (..), ila, ilaConfig)
import Data.Maybe (fromMaybe, isJust)
import Language.Haskell.TH (runIO)
import Project.FilePath
import Protocols
import Protocols.Idle
import System.FilePath
import VexRiscv

import Bittide.CircuitUtils
import Bittide.ClockControl (RelDataCount)
import Bittide.ClockControl.Callisto.Types (CallistoResult (..), ReframingState (Done))
import Bittide.ClockControl.DebugRegister (
  DebugRegisterCfg (DebugRegisterCfg),
  debugRegisterWb,
 )
import Bittide.ClockControl.Registers (ClockControlData (..), clockControlWb)
import Bittide.DoubleBufferedRam (ContentType (Blob), InitialContent (Reloadable))
import Bittide.ProcessingElement (PeConfig (..), processingElement)
import Bittide.ProcessingElement.Util (memBlobsFromElf)
import Bittide.SharedTypes (ByteOrder (BigEndian))

-- | Configuration type for software clock control.
data SwControlConfig dom mgn fsz where
  SwControlConfig ::
    ( KnownNat mgn
    , KnownNat fsz
    , 1 <= fsz
    , KnownDomain dom
    ) =>
    -- | Enable reframing?
    --
    -- N.B.: FOR TESTING USE ONLY. Reframing should eventually be handled solely within
    -- the clock control software. See issue #693.
    Signal dom Bool ->
    -- | Stability checker margin
    SNat mgn ->
    -- | Stability checker frame size
    SNat fsz ->
    SwControlConfig dom mgn fsz

{- | Instantiates a Vex RISC-V core ready to run the Callisto clock control algorithm.

The CPU is instantiated with 64KB of IMEM containing the 'clock-control' binary from
'firmware-binaries' and another 64KB of DMEM. The CPU is connected to the 'clockControlWb'
(@0xC000_0000@) and 'debugRegisterWb' (@0xE000_0000@) MMIO registers.
-}
callistoSwClockControl ::
  forall nLinks eBufBits dom margin framesize.
  ( KnownDomain dom
  , KnownNat nLinks
  , KnownNat eBufBits
  , 1 <= nLinks
  , 1 <= eBufBits
  , nLinks + eBufBits <= 32
  , 1 <= framesize
  , 1 <= DomainPeriod dom
  ) =>
  -- | CPU clock
  Clock dom ->
  -- | CPU reset
  Reset dom ->
  -- | CPU enable
  Enable dom ->
  -- | Clock control config
  SwControlConfig dom margin framesize ->
  -- | Availability mask
  Signal dom (BitVector nLinks) ->
  -- | Diff counters
  Vec nLinks (Signal dom (RelDataCount eBufBits)) ->
  Signal dom (CallistoResult nLinks)
callistoSwClockControl clk rst ena (SwControlConfig (reframe :: Signal dom Bool) mgn fsz) mask ebs =
  hwSeqX callistoSwIla callistoResult
 where
  callistoResult =
    CallistoResult
      <$> ccData.clockMod
      <*> ccData.stabilityIndications
      <*> ccData.allStable
      <*> ccData.allSettled
      <*> resultRfs

  resultRfs = fromMaybe Done <$> debugData.reframingState

  callistoSwIla :: Signal dom ()
  callistoSwIla =
    setName @"callistoSwIla"
      $ ila
        ( ilaConfig
            $ "trigger_0"
            :> "capture_0"
            :> "probe_updatePeriod"
            :> "probe_updatePeriodMin"
            :> "probe_updatePeriodMax"
            :> Nil
        )
          { depth = D16384
          }
        clk
        (unsafeToActiveLow rst)
        capture
        debugData.updatePeriod
        debugData.updatePeriodMin
        debugData.updatePeriodMax

  debugRegisterCfg = DebugRegisterCfg <$> reframe

  capture = isRising clk rst ena False (isJust <$> ccData.clockMod)

  (_, (ccData, debugData)) =
    toSignals
      ( circuit $ \jtag -> do
          [wbClockControl, wbDebug, wbDummy] <-
            withClockResetEnable clk rst ena $ processingElement peConfig -< jtag
          idleSink -< wbDummy
          [ccd0, ccd1] <-
            cSignalDupe
              <| withClockResetEnable
                clk
                rst
                ena
                (clockControlWb mgn fsz mask ebs)
              -< wbClockControl
          cm <- cSignalMap clockMod -< ccd0
          dbg <-
            withClockResetEnable clk rst enableGen
              $ debugRegisterWb debugRegisterCfg
              -< (wbDebug, cm)
          idC -< (ccd1, dbg)
      )
      (pure $ JtagIn low low low, (pure (), pure ()))
  (iMem, dMem) =
    $( do
        root <- runIO $ findParentContaining "cabal.project"
        let
          elfDir = root </> firmwareBinariesDir "riscv32imc-unknown-none-elf" Release
          elfPath = elfDir </> "clock-control"
          iSize = 64 * 1024 -- 64 KB
          dSize = 64 * 1024 -- 64 KB
        memBlobsFromElf BigEndian (Just iSize, Just dSize) elfPath Nothing
     )
  peConfig =
    PeConfig
      (0b100 :> 0b010 :> 0b110 :> 0b111 :> 0b001 :> Nil)
      (Reloadable $ Blob iMem)
      (Reloadable $ Blob dMem)
