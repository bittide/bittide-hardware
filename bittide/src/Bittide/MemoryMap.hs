-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Functor law" #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Bittide.MemoryMap where

import           Clash.Prelude
import           Bittide.Extra.Wishbone
import           Data.Maybe

type BaseAddress aw = BitVector aw
type MemoryMap slaveDevices addressWidth = Vec slaveDevices (BaseAddress addressWidth)

-- | Component that maps multiple slave devices to a single master device over the wishbone
-- bus, it assumes that the config argument contains incrementing base addresses that correspond
-- to the indexes of the slaves in the incoming slave-busses and outgoing master-busses result.
-- It routes the incoming control signals to a slave device based on
memoryMap ::
 forall dom slaveDevices bytes addressWidth .
 HiddenClockResetEnable dom =>
 (KnownNat slaveDevices, KnownNat bytes, KnownNat addressWidth) =>
 MemoryMap slaveDevices addressWidth->
 Signal dom (WishboneM2S bytes addressWidth) ->
 Signal dom (Vec slaveDevices (WishboneS2M bytes)) ->
 (Signal dom (WishboneS2M bytes), Signal dom (Vec slaveDevices (WishboneM2S bytes addressWidth)))
memoryMap config (register wishboneM2S -> master) slaves = (toMaster, toSlaves)
 where
  masterActive = strobe <$> master .&&. busCycle <$> master
  selectedSlave = getSelected . addr <$> master

  toSlaves = routeToSlaves <$> masterActive <*> selectedSlave <*> master
  toMaster = routeToMaster <$> masterActive <*> selectedSlave <*> slaves

  getSelected a = elemIndex (True, False) $ zip (init compVec) (tail compVec)
   where
     compVec = fmap (<=a) config :< False

  routeToMaster active sel slaves0
    | active    = fromMaybe wishboneS2M $ (slaves0 !!) <$> sel
    | otherwise = wishboneS2M

  routeToSlaves active sel m@WishboneM2S{..}
    | active    = fromMaybe allSlaves out
    | otherwise = allSlaves
   where
     out = (\i -> replace i toAllSlaves{busCycle, strobe, writeEnable} allSlaves) <$> sel
     newAddr = addr - (config !! (fromMaybe 0 sel))
     allSlaves = repeat toAllSlaves
     toAllSlaves = m{addr=newAddr, busCycle = False, strobe = False}
