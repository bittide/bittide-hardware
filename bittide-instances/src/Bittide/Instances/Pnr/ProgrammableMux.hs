-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.Pnr.ProgrammableMux where

import Clash.Prelude

import Bittide.Instances.Domains (Basic400)
import Bittide.Instances.Hacks (reducePins)
import Bittide.SharedTypes (withLittleEndian)
import Protocols

import qualified Bittide.ProgrammableMux as PM

programmableMuxFast ::
  Clock Basic400 -> Reset Basic400 -> Signal Basic400 Bit -> Signal Basic400 Bit
programmableMuxFast clk rst = withClock clk $ reducePins dut
 where
  dut (unbundle -> (localCounter, wbIn, linkA, linkB)) =
    bundle (wbOut, unsafeToActiveHigh rstOut, linkOut)
   where
    (((_mm, wbOut), _, _), (rstOut, linkOut)) =
      toSignals
        ( withLittleEndian
            $ withClockResetEnable clk rst enableGen
            $ PM.programmableMux @_ @32 @4 @(BitVector 64) localCounter
        )
        ((((), wbIn), linkA, linkB), ((), ()))
