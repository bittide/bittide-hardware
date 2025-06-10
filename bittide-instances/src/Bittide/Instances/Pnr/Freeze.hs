-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Bittide.Instances.Pnr.Freeze where

import BitPackC (ByteOrder (BigEndian))
import Bittide.SharedTypes (Bytes)
import Clash.Explicit.Prelude
import GHC.Stack (HasCallStack)
import Protocols
import Protocols.MemoryMap
import Protocols.Wishbone

import qualified Bittide.ClockControl.Freeze as Freeze

-- | Instance of 'Freeze.freeze'. Mostly here to show off a "real" memory map.
freezeExample ::
  forall dom.
  ( HasCallStack
  , KnownDomain dom
  ) =>
  Clock dom ->
  Reset dom ->
  Circuit
    (ConstBwd MM, Wishbone dom 'Standard 4 (Bytes 4))
    ()
freezeExample clk rst =
  let
    ?busByteOrder = BigEndian
    ?regByteOrder = BigEndian
   in
    circuit $ \(mm, wb) -> do
      Freeze.freeze clk rst
        -< (mm, wb, Fwd ebCounters, Fwd counter0, Fwd counter1, Fwd counter2)

      idC
 where
  counter0 = Freeze.counter @(Unsigned 32) clk rst enableGen 0
  counter1 = Freeze.counter @(Unsigned 32) clk rst enableGen 1
  counter2 = Freeze.counter @(Unsigned 32) clk rst enableGen 2

  -- Clash refuses to compile more clever constructs than this, as it doesn't
  -- propagate a constant to the register's reset value.
  ebCounters =
    bundle
      $ Freeze.counter @(Signed 32) clk rst enableGen 3
      :> Freeze.counter @(Signed 32) clk rst enableGen 4
      :> Freeze.counter @(Signed 32) clk rst enableGen 5
      :> Freeze.counter @(Signed 32) clk rst enableGen 6
      :> Freeze.counter @(Signed 32) clk rst enableGen 7
      :> Freeze.counter @(Signed 32) clk rst enableGen 8
      :> Freeze.counter @(Signed 32) clk rst enableGen 9
      :> Nil

freezeMM :: MemoryMap
freezeMM = getMMAny $ freezeExample @XilinxSystem clk rst
 where
  clk = clockGen
  rst = resetGen

freeze ::
  Clock XilinxSystem ->
  Reset XilinxSystem ->
  Signal XilinxSystem (WishboneM2S 4 4 (BitVector 32)) ->
  Signal XilinxSystem (WishboneS2M (BitVector 32))
freeze clk rst m2s =
  snd $ fst $ toSignals (freezeExample @XilinxSystem clk rst) (((), m2s), ())
