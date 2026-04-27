-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.Tests.ClockControlWb where

import Clash.Prelude

import Bittide.ClockControl (SpeedChange)
import Bittide.ClockControl.Registers (clockControlWb)
import Bittide.Cpus.Riscv32imc (vexRiscv0)
import Bittide.Instances.Common (PeConfigElfSource (NameOnly), emptyPeConfig, peConfigFromElf)
import Bittide.Instances.Hitl.Setup hiding (linkMask)
import Bittide.ProcessingElement
import Bittide.SharedTypes (withLittleEndian)
import Bittide.Wishbone hiding (MemoryMap)
import GHC.Stack (HasCallStack)
import Project.FilePath
import Protocols
import Protocols.Idle
import Protocols.MemoryMap
import VexRiscv (DumpVcd (NoDumpVcd))

linkMask :: BitVector LinkCount
linkMask = 0b1011011

linksOk :: BitVector LinkCount
linksOk = 0b1111000

dataCounts :: Vec LinkCount (Signed 32)
dataCounts = iterateI (satSucc SatWrap) 0

type IMemWords = DivRU (8 * 1024) 4
type DMemWords = DivRU (8 * 1024) 4

peConfigSim :: IO (PeConfig 4)
peConfigSim =
  peConfigFromElf
    (SNat @IMemWords)
    (SNat @DMemWords)
    (NameOnly "clock-control-wb")
    Release
    d0
    d0
    False
    vexRiscv0

peConfigEmpty :: PeConfig 4
peConfigEmpty = emptyPeConfig (SNat @IMemWords) (SNat @DMemWords) d0 d0 False vexRiscv0

dut ::
  (HasCallStack) =>
  PeConfig 4 ->
  Circuit (ToConstBwd Mm, ()) (Df System (BitVector 8), CSignal System (Maybe SpeedChange))
dut peConfig =
  withLittleEndian
    $ withClockResetEnable clockGen (resetGenN d2) enableGen
    $ circuit
    $ \(mm, _unit) -> do
      (uartRx, jtag) <- idleSource
      [ uartBus
        , (mmCC, ccWb)
        ] <-
        processingElement NoDumpVcd peConfig -< (mm, jtag)
      (uartTx, _uartStatus) <- uartInterfaceWb d2 d2 uartBytes -< (uartBus, uartRx)

      ccd <-
        clockControlWb
          (pure linkMask)
          (pure linksOk)
          (pure <$> dataCounts)
          -< (mmCC, ccWb)

      idC -< (uartTx, ccd)
 where

{-# OPAQUE dut #-}

dutMm :: (HasCallStack) => MemoryMap
dutMm = getMMAny $ dut peConfigEmpty

dutNoMm ::
  (HasCallStack) =>
  PeConfig 4 ->
  Circuit () (Df System (BitVector 8), CSignal System (Maybe SpeedChange))
dutNoMm = unMemmap . dut
