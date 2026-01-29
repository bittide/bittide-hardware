-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Protocols.Wishbone.Extra (
  delayWishbone,
  increaseBuswidth,
  trace,
) where

import Clash.Prelude
import Data.String.Interpolate (i)
import Protocols
import Protocols.Wishbone

import qualified Debug.Trace as Debug

data DelayWishboneState aw n
  = WaitingForManager
  | WaitingForSubordinate (WishboneM2S aw n)
  | AcknowledgingManager (WishboneS2M n)
  deriving (Generic, NFDataX)

{- | Breaks the combinatorial path between a Wishbone manager and subordinate by inserting
a Moore machine. It introduces two cycles of delay for each transaction, one to forward
the request from manager to subordinate, and one to forward the response from subordinate
to manager.
-}
delayWishbone ::
  forall dom aw n.
  (HiddenClockResetEnable dom, KnownNat aw, KnownNat n, 1 <= n) =>
  Circuit (Wishbone dom 'Standard aw n) (Wishbone dom 'Standard aw n)
delayWishbone = Circuit go
 where
  go ::
    ( Signal dom (WishboneM2S aw n)
    , Signal dom (WishboneS2M n)
    ) ->
    ( Signal dom (WishboneS2M n)
    , Signal dom (WishboneM2S aw n)
    )
  go = mooreB mooreTransfer mooreOut WaitingForManager
   where
    mooreTransfer ::
      DelayWishboneState aw n ->
      (WishboneM2S aw n, WishboneS2M n) ->
      DelayWishboneState aw n
    mooreTransfer WaitingForManager (m2s, _s2m)
      | m2s.busCycle && m2s.strobe = WaitingForSubordinate m2s
    mooreTransfer (WaitingForSubordinate _) (_m2s, s2m)
      | hasTerminateFlag s2m = AcknowledgingManager s2m
    mooreTransfer (AcknowledgingManager _) _ = WaitingForManager
    mooreTransfer s _ = s

    mooreOut ::
      DelayWishboneState aw n ->
      (WishboneS2M n, WishboneM2S aw n)
    mooreOut WaitingForManager = (emptyWishboneS2M, emptyWishboneM2S)
    mooreOut (WaitingForSubordinate m2s) = (emptyWishboneS2M, m2s)
    mooreOut (AcknowledgingManager s2m) = (s2m, emptyWishboneM2S)

increaseBuswidth ::
  forall dom mode aw width power.
  (HiddenClockResetEnable dom, KnownNat aw, KnownNat width, KnownNat power, power <= aw) =>
  Circuit
    (Wishbone dom mode aw width)
    (Wishbone dom mode (aw - power) (2 ^ power * width))
increaseBuswidth = Circuit (unbundle . fmap go . bundle)
 where
  go (m2sLeft, s2mRight) = (s2mLeft, m2sRight)
   where
    m2sRight =
      m2sLeft
        { addr = newAddr
        , writeData = newWriteData
        , busSelect = newBusSelect
        }
    s2mLeft =
      s2mRight
        { readData = newReadData
        }

    newAddr = addrMsbs
    bitShift = natToNum @width * fromIntegral addrLsbs
    (addrMsbs, addrLsbs :: BitVector power) = bitCoerce (m2sLeft.addr)
    newWriteData = pack (repeat m2sLeft.writeData)
    newBusSelect = shiftL (resize m2sLeft.busSelect) bitShift
    newReadData = (unpack s2mRight.readData) !! addrLsbs

trace ::
  (KnownNat aw, KnownNat nBytes) =>
  String ->
  Circuit (Wishbone dom mode aw nBytes) (Wishbone dom mode aw nBytes)
trace msg = Circuit (unbundle . fmap go . bundle)
 where
  go (m2s, s2m)
    | m2s.busCycle && m2s.strobe =
        (Debug.trace ([i| #{msg} M2S: #{showX m2s}, S2M: #{showX s2m} |]) s2m, m2s)
    | otherwise = (s2m, m2s)
