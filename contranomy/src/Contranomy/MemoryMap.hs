{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Functor law" #-}
module Contranomy.MemoryMap where

import           Clash.Prelude
import           Contranomy.Wishbone
import           Data.Maybe

type MemoryMap slaveDevices addressWidth = Vec slaveDevices (BitVector addressWidth)

memoryMap :: forall dom slaveDevices bytes addressWidth . (HiddenClockResetEnable dom) =>
  (KnownNat slaveDevices, KnownNat bytes, KnownNat addressWidth) =>
  MemoryMap slaveDevices addressWidth->
  Signal dom (WishboneM2S bytes addressWidth) ->
  Signal dom (Vec slaveDevices (WishboneS2M bytes)) ->
  (Signal dom (WishboneS2M bytes), Signal dom (Vec slaveDevices (WishboneM2S bytes addressWidth)))
memoryMap config (register wishboneM2S -> master) slaves = (toMaster, toSlaves)
 where
  masterActive = strobe <$> master .&&.  busCycle <$> master
  selectedSlave = getSelected . addr <$> master

  toSlaves = mux masterActive
    (routeToSlaves <$> selectedSlave <*> master)
    (pure $ repeat wishboneM2S)

  toMaster = mux masterActive
    ((!!) <$> slaves <*> selectedSlave)
    (pure wishboneS2M)

  getSelected a = fromMaybe minBound $ elemIndex (True, False) $ zip (init compVec) (tail compVec)
   where
     compVec = fmap (<=a) config :< False
  routeToSlaves sel m = replace sel m{addr=newAddr} (repeat wishboneM2S)
   where
     newAddr = addr m - (config !! sel)
