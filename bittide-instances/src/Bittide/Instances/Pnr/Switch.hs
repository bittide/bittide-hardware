-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Pnr.Switch where

import Clash.Prelude

import Bittide.Calendar
import Bittide.Instances.Domains (Basic200)
import Bittide.Instances.Hacks (reducePins)
import Bittide.SharedTypes
import Bittide.Switch
import GHC.Stack (HasCallStack)
import Protocols
import Protocols.MemoryMap
import Protocols.Wishbone

switchExample ::
  (HasCallStack) =>
  Clock Basic200 ->
  Reset Basic200 ->
  Circuit
    ( ConstBwd MM
    , ( Vec 16 (CSignal Basic200 (BitVector 64))
      , Wishbone Basic200 'Standard 27 (Bytes 4) -- calendar interface
      )
    )
    ( Vec 16 (CSignal Basic200 (BitVector 64))
    , CSignal Basic200 (Vec 16 (Index 17))
    )
switchExample clk rst =
  withBittideByteOrder
    $ withClockResetEnable clk syncRst enableGen
    {- The 12s here and below are so that the generated Rust code works. At time
    of writing, the generator makes two separate device-specific types for 'ValidEntry'
    since they have differing repetition bit widths. To fix this, all tests are
    being set to a width of 12.
    -}
    $ switchC (CalendarConfig (SNat @256) d12 calActive calShadow)
 where
  syncRst = resetSynchronizer clk rst
  calActive :: Vec 2 (ValidEntry (Vec 16 (Index 17)) 12)
  calActive =
    $( lift
        $ ( ValidEntry{veEntry = fmap resize indicesI, veRepeat = 8} ::
              (ValidEntry (Vec 16 (Index 17)) 12)
          )
        :> ValidEntry{veEntry = reverse $ fmap resize indicesI, veRepeat = 16}
        :> Nil
     )
  calShadow :: Vec 16 (ValidEntry (Vec 16 (Index 17)) 12)
  calShadow =
    $( lift
        $ iterate
          d16
          (\ve -> ve{veEntry = fmap succ ve.veEntry, veRepeat = succ ve.veRepeat})
          (ValidEntry{veEntry = repeat 0, veRepeat = 0} :: (ValidEntry (Vec 16 (Index 17)) 12))
     )
{-# OPAQUE switchExample #-}

switchExampleReducedPins ::
  Clock Basic200 ->
  Reset Basic200 ->
  Signal Basic200 Bit ->
  Signal Basic200 Bit
switchExampleReducedPins clk rst =
  withClock clk $ reducePins dut
 where
  dut (unbundle -> (unbundle -> linksIn, m2s)) = bundle (s2m, bundle linksOut, sels)
   where
    ((_, s2m), (linksOut, sels)) =
      toSignals (unMemmap $ switchExample clk rst) ((linksIn, m2s), (repeat (), ()))
{-# OPAQUE switchExampleReducedPins #-}
