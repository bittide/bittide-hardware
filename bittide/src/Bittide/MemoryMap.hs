{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
{-# LANGUAGE NamedFieldPuns #-}
module Bittide.MemoryMap where

import Clash.Prelude

import Contranomy.Wishbone
import Data.Maybe

type MemoryMap slaveDevices addressWidth = Vec slaveDevices (BitVector addressWidth)
memoryMap ::
  (KnownNat slaveDevices, KnownNat bytes, KnownNat addressWidth) =>
  MemoryMap slaveDevices addressWidth ->
  WishboneM2S bytes addressWidth ->
  Vec slaveDevices (WishboneS2M bytes) ->
  (WishboneS2M bytes, Vec slaveDevices (WishboneM2S bytes addressWidth))
memoryMap config master@WishboneM2S{addr} slaves = (toMaster, toSlaves)
 where
  idleSlaves = repeat idleM2S
  selectedSlave = fromMaybe maxBound $ elemIndex True $ fmap (>=addr) config
  newAddr = addr - (config !! selectedSlave)
  toSlaves = replace selectedSlave master{addr = newAddr} idleSlaves
  toMaster = slaves !! selectedSlave

idleM2S :: (KnownNat bytes, KnownNat addressWidth) => WishboneM2S bytes addressWidth
idleM2S = WishboneM2S
  { addr = 0
  , writeData = 0
  , busSelect = 0
  , busCycle = False
  , strobe = False
  , writeEnable = False
  , cycleTypeIdentifier = Classic
  , burstTypeExtension = LinearBurst
  }
