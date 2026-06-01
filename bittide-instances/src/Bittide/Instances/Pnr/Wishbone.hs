-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Pnr.Wishbone where

import Clash.Prelude
import Protocols

import Bittide.Instances.Domains (Basic400)
import Bittide.Instances.Hacks (reducePins)
import Clash.Cores.Xilinx (withXilinx)
import Data.Bifunctor (first)

import qualified Bittide.Wishbone as Wb

arbiterFast :: Clock Basic400 -> Reset Basic400 -> Signal Basic400 Bit -> Signal Basic400 Bit
arbiterFast clk rst = withClock clk $ reducePins dut
 where
  dut (unbundle -> (wb0Ins, wb1Out)) =
    bundle
      $ first bundle
      $ toSignals
        ( withXilinx
            $ withClockResetEnable clk rst enableGen
            $ Wb.arbiter @_ @32 @4 @2
        )
        (unbundle wb0Ins, wb1Out)
