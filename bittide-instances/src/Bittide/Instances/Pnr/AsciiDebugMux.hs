-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Pnr.AsciiDebugMux where

import Clash.Prelude
import Protocols

import Bittide.Instances.Domains (Basic400)
import Bittide.Instances.Hacks (reducePins)
import Bittide.SharedTypes (Byte)
import Data.Bifunctor (first)

import qualified Bittide.Df as Df

asciiDebugMuxExample ::
  (KnownDomain dom) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  Circuit (Vec 3 (Df dom Byte)) (Df dom Byte)
asciiDebugMuxExample clk rst ena =
  withClockResetEnable clk rst ena
    $ Df.asciiDebugMux
      (SNat @1024)
      ((0xA :> Nil) :> (0xB :> Nil) :> (0xC :> Nil) :> Nil)

asciiDebugMux ::
  Clock System ->
  Reset System ->
  Enable System ->
  Circuit (Vec 3 (Df System Byte)) (Df System Byte)
asciiDebugMux = asciiDebugMuxExample

asciiDebugMuxFast :: Clock Basic400 -> Reset Basic400 -> Signal Basic400 Bit -> Signal Basic400 Bit
asciiDebugMuxFast clk rst = withClock clk $ reducePins dut
 where
  dut (unbundle -> (inps, ack)) =
    bundle
      $ first bundle
      $ toSignals
        (asciiDebugMuxExample clk rst enableGen)
        (unbundle inps, ack)
