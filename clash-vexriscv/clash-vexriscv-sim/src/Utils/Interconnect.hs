-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RecordWildCards #-}

-- GHC doesn't think exhaustive matches on Vectors are exhaustive..
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Utils.Interconnect where

import Clash.Prelude
import Protocols.Wishbone

-- | A single-master interconnect for two slave circuits.
-- 
--   The addresses have to be in ascending order, the element order
--   of the input and output vectors are the same.
interconnectTwo ::
  (HiddenClockResetEnable dom) =>
  Signal dom (WishboneM2S 32 4 (BitVector 32)) ->
  Vec 2 (BitVector 32, Signal dom (WishboneS2M (BitVector 32))) ->
  -- ^ sorted by address
  ( Signal dom (WishboneS2M (BitVector 32))
  , Signal dom (Vec 2 (WishboneM2S 32 4 (BitVector 32))))
interconnectTwo m2s ((aAddr', aS2M') :> (bAddr', bS2M') :> Nil) =
  unbundle (go <$> m2s <*> pure aAddr' <*> aS2M' <*> pure bAddr' <*> bS2M')
  where
    go ::
      WishboneM2S 32 4 (BitVector 32) ->
      BitVector 32 ->
      WishboneS2M (BitVector 32) ->
      BitVector 32 ->
      WishboneS2M (BitVector 32) ->
      ( WishboneS2M (BitVector 32),
        Vec 2 (WishboneM2S 32 4 (BitVector 32))
      )
    go m@WishboneM2S{..} aAddr aS2M bAddr bS2M
      | not (busCycle && strobe) =
          (emptyWishboneS2M, m :> m :> Nil)
      | addr >= bAddr =
          (bS2M, emptyWishboneM2S :> m { addr = addr - bAddr } :> Nil)
      | addr >= aAddr =
          (aS2M, m { addr = addr - aAddr } :> emptyWishboneM2S :> Nil)
      | otherwise     =
          ( emptyWishboneS2M { err = True },
            emptyWishboneM2S :> emptyWishboneM2S :> Nil
          )
