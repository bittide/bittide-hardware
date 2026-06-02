-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Pnr.WbStorage where

import Clash.Prelude
import Protocols

import Bittide.DoubleBufferedRam (wbStorage)
import Bittide.Instances.Domains (Basic300)
import Bittide.Instances.Hacks (reducePins)
import Protocols.Experimental.Wishbone
import Protocols.MemoryMap (Mm, unMemmap)

wbStorageExample ::
  forall dom depth.
  (HiddenClockResetEnable dom, 1 <= depth) =>
  SNat depth ->
  Circuit (ToConstBwd Mm, Wishbone dom 'Standard 32 4) ()
wbStorageExample depth = wbStorage "SampleMemory" depth Nothing

wbStorageFast :: Clock Basic300 -> Reset Basic300 -> Signal Basic300 Bit -> Signal Basic300 Bit
wbStorageFast clk rst = withClock clk $ reducePins dut
 where
  dut wbIn =
    fst
      $ toSignals
        (unMemmap $ withClockResetEnable clk rst enableGen $ wbStorageExample (SNat @36_000))
        (wbIn, ())
