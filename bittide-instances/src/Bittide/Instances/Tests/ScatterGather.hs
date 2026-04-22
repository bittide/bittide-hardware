-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.Tests.ScatterGather where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClockResetEnable, withClockResetEnable)

import GHC.Stack (HasCallStack)
import Protocols
import Protocols.Idle
import Protocols.MemoryMap
import VexRiscv (DumpVcd (NoDumpVcd))

import Bittide.Calendar
import Bittide.Cpus.Riscv32imc (vexRiscv0)
import Bittide.Instances.Common (emptyPeConfig)
import Bittide.ProcessingElement
import Bittide.ScatterGather
import Bittide.SharedTypes (withLittleEndian)
import Bittide.Wishbone

genIncrementingCalendar ::
  forall size repititionBits.
  (KnownNat size, KnownNat repititionBits) =>
  Calendar size (Index size) repititionBits
genIncrementingCalendar = iterateI f (ValidEntry 0 0)
 where
  f (ValidEntry i _) = ValidEntry (succ i) 0

-- Padding is required to increase the duration of a metacycle, giving the CPU
-- enough time to write to the gather memory and read from the scatter memory.
-- The calendar for the scatter unit is delayed by one cycle.
padding :: Unsigned 12
padding = 512
incrementingCal :: Vec 16 (ValidEntry (Index 16) 12)
incrementingCal = genIncrementingCalendar @16
gatherCal :: Vec 17 (ValidEntry (Index 16) 12)
gatherCal = incrementingCal :< ValidEntry maxBound (padding - 1 :: Unsigned 12)
scatterCal :: Vec 18 (ValidEntry (Index 16) 12)
scatterCal = (ValidEntry 0 0 :> incrementingCal) :< ValidEntry maxBound (padding - 2)

scatterConfig :: forall n. (KnownNat n) => ScatterConfig 4 n
scatterConfig = ScatterConfig SNat $ CalendarConfig d32 (SNat @12) scatterCal scatterCal
gatherConfig :: forall n. (KnownNat n) => GatherConfig 4 n
gatherConfig = GatherConfig SNat $ CalendarConfig d32 (SNat @12) gatherCal gatherCal

dutMM :: (HasCallStack) => Protocols.MemoryMap.MemoryMap
dutMM =
  getMMAny
    $ withClockResetEnable @System clockGen (resetGenN d2) enableGen
    $ dutWithPeConfig
    $ emptyPeConfig (SNat @IMemWords) (SNat @DMemWords) d0 d0 False vexRiscv0

-- | Parameterized DUT that loads a specific firmware binary.
dutWithPeConfig ::
  (HasCallStack, HiddenClockResetEnable dom) =>
  PeConfig 7 ->
  Circuit (ToConstBwd Mm, ()) (Df dom (BitVector 8))
dutWithPeConfig peConfig = withLittleEndian $ circuit $ \(mm, _unit) -> do
  (uartRx, jtagIdle) <- idleSource
  [ uartBus
    , wbSu
    , wbGu
    , wbSuCal
    , wbGuCal
    ] <-
    processingElement NoDumpVcd peConfig -< (mm, jtagIdle)
  (uartTx, _uartStatus) <- uartInterfaceWb d16 d2 uartBytes -< (uartBus, uartRx)
  Fwd link <- gatherUnitWbC gatherConfig -< (wbGu, wbGuCal)
  scatterUnitWbC scatterConfig link -< (wbSu, wbSuCal)
  idC -< uartTx
{-# OPAQUE dutWithPeConfig #-}

type IMemWords = DivRU (16 * 1024) 4
type DMemWords = DivRU (16 * 1024) 4
