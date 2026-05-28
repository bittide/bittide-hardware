-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.Pnr.ClockControl where

import Clash.Prelude
import Protocols

import Bittide.ClockControl (SpeedChange)
import Bittide.ClockControl.CallistoSw (SwcccInternalBusses, callistoSwClockControlC)
import Bittide.Instances.Domains (Basic300)
import Bittide.Instances.Hacks (reducePins)
import Bittide.ProcessingElement (PeConfig (..))
import Bittide.SharedTypes (Bitbone, withLittleEndian)
import Bittide.Sync (Sync)
import Clash.Cores.Xilinx (withXilinx)
import Protocols.MemoryMap (ignoreMM)
import VexRiscv (DumpVcd (..), Jtag)

import qualified Bittide.Cpus.Riscv32imc as Riscv32imc

type LinkCount = 6

ccConfig :: PeConfig SwcccInternalBusses
ccConfig =
  PeConfig
    { cpu = Riscv32imc.vexRiscv2
    , depthI = SNat @(Div (8 * 1024) 4)
    , depthD = SNat @(Div (16 * 1024) 4)
    , initI = Nothing
    , initD = Nothing
    , iBusTimeout = d0
    , dBusTimeout = d0
    , includeIlaWb = False
    }

-- | A very much simplified version of 'callistoSwClockControlC' which only uses one clock domain.
dut ::
  forall dom.
  ( HiddenClockResetEnable dom
  , HasSynchronousReset dom
  , HasDefinedInitialValues dom
  , 1 <= DomainPeriod dom
  ) =>
  Circuit
    ( Bitbone dom 32
    , Jtag dom
    , CSignal dom (BitVector LinkCount)
    , CSignal dom (BitVector LinkCount)
    )
    ( Sync dom dom
    , CSignal dom (Maybe SpeedChange)
    )
dut = withXilinx $ withLittleEndian $ circuit $ \(muBus, jtag, linkMask, Fwd linksOk) -> do
  outerMm <- ignoreMM -< ()
  innerMm <- ignoreMM -< ()
  (sync, speedChange, []) <-
    callistoSwClockControlC
      hasClock
      hasReset
      (repeat hasClock)
      (repeat hasReset)
      NoDumpVcd
      ccConfig
      -< (outerMm, (innerMm, muBus), (jtag, linkMask, Fwd linksOk))
  idC -< (sync, speedChange)

callistoSwClockControlCFast ::
  Clock Basic300 -> Reset Basic300 -> Signal Basic300 Bit -> Signal Basic300 Bit
callistoSwClockControlCFast clk rst = withClock clk $ reducePins go
 where
  go (unbundle -> (wbIn, jtagIn, linkMask, linksOk, syncIn)) =
    bundle (wbOut, jtagOut, syncOut, speedChange)
   where
    ((wbOut, jtagOut, _, _), (syncOut, speedChange)) =
      toSignals
        (withClockResetEnable clk rst enableGen dut)
        ((wbIn, jtagIn, linkMask, linksOk), (syncIn, ()))
