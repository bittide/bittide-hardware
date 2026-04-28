-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Tests.SwitchDemoProcessingElement where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClockResetEnable, withClockResetEnable)

import Protocols
import Protocols.Idle
import Protocols.MemoryMap
import VexRiscv (DumpVcd (NoDumpVcd))

import Bittide.Cpus.Riscv32imc (vexRiscv0)
import Bittide.Instances.Common (emptyPeConfig)
import Bittide.ProcessingElement
import Bittide.SharedTypes (withLittleEndian)
import Bittide.SwitchDemoProcessingElement
import Bittide.Wishbone hiding (MemoryMap)

{- | A simulation-only design containing two `switchDemoPeWb`s connected to a single
VexRiscV. The VexRiscV runs the `switch_demo_pe_test` binary from `firmware-binaries`.
-}
dutWithMm ::
  forall dom.
  ( HiddenClockResetEnable dom
  , 1 <= DomainPeriod dom
  ) =>
  -- | Procesing element configuration
  PeConfig 6 ->
  -- | Fake DNA (used to identify the different PEs)
  Signal dom (BitVector 96) ->
  -- | Fake DNA (used to identify the different PEs)
  Signal dom (BitVector 96) ->
  Circuit (ToConstBwd Mm, ()) (Df dom (BitVector 8))
dutWithMm peConfig dnaA dnaB = withLittleEndian $ circuit $ \(mm, _unit) -> do
  (uartRx, jtagIdle) <- idleSource
  [ uartBus
    , (mmTime, timeBus)
    , (mmA, peBusA)
    , (mmB, peBusB)
    ] <-
    processingElement NoDumpVcd peConfig -< (mm, jtagIdle)
  (uartTx, _uartStatus) <- uartInterfaceWb d16 d2 uartBytes -< (uartBus, uartRx)
  Fwd localCounter <- timeWb Nothing -< (mmTime, timeBus)

  Fwd linkAB <- switchDemoPeWb d2 localCounter (Just <$> dnaA) linkBA -< (mmA, peBusA)

  Fwd linkBA <- switchDemoPeWb d2 localCounter (Just <$> dnaB) linkAB -< (mmB, peBusB)
  idC -< uartTx

type IMemWords = DivRU (16 * 1024) 4
type DMemWords = DivRU (16 * 1024) 4

dutMm :: MemoryMap
dutMm =
  getMMAny
    $ withClockResetEnable @System clockGen (resetGenN d2) enableGen
    $ dutWithMm emptyPeCfg (pure 0) (pure 0)
 where
  emptyPeCfg = emptyPeConfig (SNat @IMemWords) (SNat @DMemWords) d0 d0 False vexRiscv0

dut ::
  forall dom.
  ( HiddenClockResetEnable dom
  , 1 <= DomainPeriod dom
  ) =>
  -- | Procesing element configuration
  PeConfig 6 ->
  -- | Fake DNA (used to identify the different PEs)
  Signal dom (BitVector 96) ->
  -- | Fake DNA (used to identify the different PEs)
  Signal dom (BitVector 96) ->
  Circuit () (Df dom (BitVector 8))
dut peConfig dnaA dnaB = unMemmap $ dutWithMm peConfig dnaA dnaB
