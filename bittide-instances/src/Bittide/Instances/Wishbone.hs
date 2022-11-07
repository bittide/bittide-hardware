-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Wishbone where

import Clash.Prelude

import Bittide.Wishbone
import Bittide.Instances.Domains
import Protocols.Wishbone
import Bittide.SharedTypes
import Bittide.Instances.Hacks (reducePins)

type NSlaves = 32
type WishboneAddrWidth = 32
type WishboneWidth = 4

memMap :: (KnownNat addrWidth, KnownNat nSlaves ) => MemoryMap nSlaves addrWidth
memMap = iterateI (+0x10000) 0

{-# ANN singleMasterInterconnect_32_32
  (Synthesize
    { t_name = "singleMasterInterconnect_32_32"
    , t_inputs =
        [ PortName "clk"
        , PortName "rst"
        , PortName "fromMaster"
        , PortName "fromSlaves"
        ]
    , t_output = PortProduct ""
        [ PortName "toMaster"
        , PortName "toSlaves"
        ]
    }
  )#-}

singleMasterInterconnect_32_32 ::
  Clock Basic200 ->
  Reset Basic200 ->
  Signal Basic200 (WishboneM2S WishboneAddrWidth WishboneWidth (Bytes WishboneWidth)) ->
  Signal Basic200 (Vec NSlaves (WishboneS2M (Bytes WishboneWidth))) ->
  ( Signal Basic200 (WishboneS2M (Bytes WishboneWidth))
  , Signal Basic200 (Vec NSlaves (WishboneM2S WishboneAddrWidth WishboneWidth (Bytes WishboneWidth)))
  )
singleMasterInterconnect_32_32 clk rst =
  withClockResetEnable clk rst enableGen $ singleMasterInterconnect' memMap

singleMasterInterconnect_32_32ReducedPins ::
  Clock Basic200 -> Reset Basic200 -> Signal Basic200 Bit -> Signal Basic200 Bit
singleMasterInterconnect_32_32ReducedPins clk rst =
  withClockResetEnable clk rst enableGen (reducePins singleMasterInterconnect'')
 where
  singleMasterInterconnect'' (unbundle -> (a,b)) =
    bundle $ singleMasterInterconnect_32_32 clk rst a b
