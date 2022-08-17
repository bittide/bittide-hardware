-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0


{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Bittide.Wishbone where

import           Clash.Prelude
import           Bittide.Extra.Wishbone
import           Data.Maybe

{-# ANN module "HLint: ignore Functor law" #-}

type MemoryMap nSlaves addressWidth = Vec nSlaves (BitVector addressWidth)

{-# NOINLINE singleMasterInterconnect #-}
-- | Component that maps multiple slave devices to a single master device over the wishbone
-- bus, it assumes that the config argument contains increasing base addresses that correspond
-- to the indexes of the slaves in the incoming slave-busses and outgoing master-busses.
-- It routes the incoming control signals to a slave device based on
singleMasterInterconnect ::
 forall dom nSlaves bytes addressWidth .
 ( HiddenClockResetEnable dom
 , KnownNat nSlaves, KnownNat bytes, KnownNat addressWidth) =>
 MemoryMap nSlaves addressWidth->
 Signal dom (WishboneM2S bytes addressWidth) ->
 Signal dom (Vec nSlaves (WishboneS2M bytes)) ->
 (Signal dom (WishboneS2M bytes), Signal dom (Vec nSlaves (WishboneM2S bytes addressWidth)))
singleMasterInterconnect config (register emptyWishboneM2S -> master) slaves =
  (toMaster, toSlaves)
 where
  masterActive = strobe <$> master .&&. busCycle <$> master
  selectedSlave = getSelected . addr <$> master

  toSlaves = routeToSlaves <$> masterActive <*> selectedSlave <*> master
  toMaster = routeToMaster <$> masterActive <*> selectedSlave <*> slaves

  -- compVec is a vector of comparison(<=) results where the addresses in config are
  -- compared to the wishbone address. getSelected returns the location of the last
  -- comparison that returns True. It depends on the assumption that config is a list
  -- of increasing addresses (config[i] < config[i+1] holds for all i).
  getSelected a = elemIndex (True, False) $ zip (init compVec) (tail compVec)
   where
     compVec = fmap (<=a) config :< False

  routeToMaster active sel slaves0
    | active    = maybe wishboneS2M (slaves0 !!) sel
    | otherwise = wishboneS2M

  routeToSlaves active sel m@WishboneM2S{..}
    | active    = fromMaybe allSlaves out
    | otherwise = allSlaves
   where
     out = (\i -> replace i toAllSlaves{busCycle, strobe, writeEnable} allSlaves) <$> sel
     newAddr = addr - maybe 0 (config !!) sel
     allSlaves = repeat toAllSlaves
     toAllSlaves = m{addr=newAddr, busCycle = False, strobe = False}
