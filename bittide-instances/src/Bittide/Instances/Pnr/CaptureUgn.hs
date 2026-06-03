-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Pnr.CaptureUgn where

import Clash.Prelude
import Protocols

import Bittide.Instances.Domains (Basic400)
import Bittide.Instances.Hacks (reducePins)
import Bittide.SharedTypes (withLittleEndian)
import Protocols.MemoryMap (unMemmap)

import qualified Bittide.CaptureUgn as Ugn

captureUgnFast ::
  Clock Basic400 -> Reset Basic400 -> Signal Basic400 Bit -> Signal Basic400 Bit
captureUgnFast clk rst = withClock clk $ reducePins dut
 where
  dut (unbundle -> (localCounter, linkIn, wbIn)) =
    bundle
      $ toSignals
        ( unMemmap
            $ withLittleEndian
            $ withClockResetEnable clk rst enableGen
            $ Ugn.captureUgns @_ @_ @32 localCounter (bundle (linkIn :> Nil))
        )
        (wbIn, ())

sendUgnFast ::
  Clock Basic400 -> Reset Basic400 -> Signal Basic400 Bit -> Signal Basic400 Bit
sendUgnFast clk rst = withClock clk $ reducePins dut
 where
  dut (unbundle -> (localCounter, sampling, linkIn)) =
    withClockResetEnable clk rst enableGen $ Ugn.sendUgn localCounter sampling linkIn
