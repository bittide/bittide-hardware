-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.Tests.ScatterGather where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClockResetEnable, withClockResetEnable)

import Clash.Class.BitPackC (ByteOrder (BigEndian))
import GHC.Stack (HasCallStack)
import Project.FilePath
import Protocols
import Protocols.Idle
import Protocols.MemoryMap
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import VexRiscv (DumpVcd (NoDumpVcd))

import Bittide.Calendar
import Bittide.Cpus.Riscv32imc (vexRiscv0)
import Bittide.DoubleBufferedRam
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.ScatterGather
import Bittide.SharedTypes (withBittideByteOrder)
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
  (\(SimOnly mm, _) -> mm)
    $ withClockResetEnable @System clockGen (resetGenN d2) enableGen
    $ toSignals (dutWithBinary "") ((), pure $ deepErrorX "memoryMap")

-- | Parameterized DUT that loads a specific firmware binary.
dutWithBinary ::
  (HasCallStack, HiddenClockResetEnable dom) =>
  String ->
  Circuit (ConstBwd MM) (Df dom (BitVector 8))
dutWithBinary binaryName = withBittideByteOrder $ circuit $ \mm -> do
  (uartRx, jtagIdle) <- idleSource
  [ uartBus
    , wbSu
    , wbGu
    , wbSuCal
    , wbGuCal
    ] <-
    processingElement NoDumpVcd (peConfig binaryName) -< (mm, jtagIdle)
  (uartTx, _uartStatus) <- uartInterfaceWb d16 d2 uartBytes -< (uartBus, uartRx)
  Fwd link <- gatherUnitWbC gatherConfig -< (wbGu, wbGuCal)
  scatterUnitWbC scatterConfig link -< (wbSu, wbSuCal)
  idC -< uartTx
 where
  peConfig binary = unsafePerformIO $ do
    root <- findParentContaining "cabal.project"
    let
      elfDir = root </> firmwareBinariesDir "riscv32imc" Release
      elfPath = elfDir </> binary
    pure
      PeConfig
        { cpu = vexRiscv0
        , initI =
            Reloadable @IMemWords
              $ Vec
              $ unsafePerformIO
              $ vecFromElfInstr BigEndian elfPath
        , initD =
            Reloadable @DMemWords
              $ Vec
              $ unsafePerformIO
              $ vecFromElfData BigEndian elfPath
        , iBusTimeout = d0 -- No timeouts on the instruction bus
        , dBusTimeout = d0 -- No timeouts on the data bus
        , includeIlaWb = False
        }
{-# OPAQUE dutWithBinary #-}

type IMemWords = DivRU (64 * 1024) 4
type DMemWords = DivRU (32 * 1024) 4
