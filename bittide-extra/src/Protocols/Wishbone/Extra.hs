-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Protocols.Wishbone.Extra (delayWishboneC) where

import Clash.Prelude
import Protocols
import Protocols.Wishbone

data DelayWishboneState aw n a
  = WaitingForManager
  | WaitingForSubordinate (WishboneM2S aw n a)
  | AcknowledgingManager (WishboneS2M a)
  deriving (Generic, NFDataX)

{- | Breaks the combinatorial path between a Wishbone manager and subordinate by inserting
a Moore machine. It introduces two cycles of delay for each transaction, one to forward
the request from manager to subordinate, and one to forward the response from subordinate
to manager.
-}
delayWishboneC ::
  forall dom aw a.
  (HiddenClockResetEnable dom, KnownNat aw, NFDataX a, BitPack a) =>
  Circuit (Wishbone dom 'Standard aw a) (Wishbone dom 'Standard aw a)
delayWishboneC = Circuit go
 where
  go ::
    forall n.
    (KnownNat n, n ~ BitSize a `DivRU` 8) =>
    ( Signal dom (WishboneM2S aw n a)
    , Signal dom (WishboneS2M a)
    ) ->
    ( Signal dom (WishboneS2M a)
    , Signal dom (WishboneM2S aw n a)
    )
  go = mooreB mooreTransfer mooreOut WaitingForManager
   where
    mooreTransfer ::
      DelayWishboneState aw n a ->
      (WishboneM2S aw n a, WishboneS2M a) ->
      DelayWishboneState aw n a
    mooreTransfer WaitingForManager (m2s, _s2m)
      | m2s.busCycle && m2s.strobe = WaitingForSubordinate m2s
    mooreTransfer (WaitingForSubordinate _) (_m2s, s2m)
      | hasTerminateFlag s2m = AcknowledgingManager s2m
    mooreTransfer (AcknowledgingManager _) _ = WaitingForManager
    mooreTransfer s _ = s

    mooreOut ::
      DelayWishboneState aw n a ->
      (WishboneS2M a, WishboneM2S aw n a)
    mooreOut WaitingForManager = (emptyWishboneS2M, emptyWishboneM2S)
    mooreOut (WaitingForSubordinate m2s) = (emptyWishboneS2M, m2s)
    mooreOut (AcknowledgingManager s2m) = (s2m, emptyWishboneM2S)
