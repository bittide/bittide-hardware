-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Pnr.Time where

import Clash.Prelude
import Protocols

import Bittide.Instances.Domains (Basic350)
import Bittide.Instances.Hacks (reducePins)
import Bittide.SharedTypes (withLittleEndian)
import Protocols.MemoryMap (unMemmap)

import qualified Bittide.Wishbone as Wb

timeWbFast ::
  Clock Basic350 -> Reset Basic350 -> Signal Basic350 Bit -> Signal Basic350 Bit
timeWbFast clk rst = withClock clk $ reducePins dut
 where
  dut (unbundle -> (lc, wbIn)) =
    bundle
      $ toSignals
        ( unMemmap
            $ withLittleEndian
            $ withClockResetEnable clk rst enableGen
            $ Wb.timeWb @_ @32 (Just lc)
        )
        (wbIn, ())
