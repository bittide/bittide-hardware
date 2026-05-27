-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.Pnr.WireDemoProcessingElement where

import Clash.Prelude
import Protocols

import Bittide.Instances.Domains (Basic400)
import Bittide.Instances.Hacks (reducePins)
import Bittide.WireDemoProcessingElement (wireDemoPe)

wireDemoPeFast :: Clock Basic400 -> Reset Basic400 -> Signal Basic400 Bit -> Signal Basic400 Bit
wireDemoPeFast clk rst = withClock clk $ reducePins dut
 where
  dut (unbundle -> (maybeDna, localCounter, linksIn, readIndex, writeIndex)) =
    go
      $ toSignals
        (withClock clk $ wireDemoPe @_ @6 @64 rst maybeDna localCounter)
        ((unbundle linksIn, readIndex, writeIndex), (units, ()))
   where
    go ((_, _, _), (linksOut, writtenData)) = bundle (bundle linksOut, writtenData)
