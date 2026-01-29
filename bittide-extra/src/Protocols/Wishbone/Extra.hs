-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Protocols.Wishbone.Extra (
  delayWishbone,
  delayWishboneMm,
  increaseBuswidth,
  trace,
  mapMm,
  strictM2S,
  strictS2M,
) where

import Clash.Prelude
import Data.String.Interpolate (i)
import Protocols
import Protocols.Wishbone

import qualified Debug.Trace as Debug
import qualified Protocols.MemoryMap as Mm

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
delayWishbone ::
  forall dom aw a.
  (HiddenClockResetEnable dom, KnownNat aw, NFDataX a, BitPack a) =>
  Circuit (Wishbone dom 'Standard aw a) (Wishbone dom 'Standard aw a)
delayWishbone = Circuit go
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

delayWishboneMm ::
  forall dom aw a.
  (HiddenClockResetEnable dom, KnownNat aw, NFDataX a, BitPack a) =>
  Circuit
    (ToConstBwd Mm.Mm, Wishbone dom 'Standard aw a)
    (ToConstBwd Mm.Mm, Wishbone dom 'Standard aw a)
delayWishboneMm = circuit $ \(mm, wbIn) -> do
  wbOut <- delayWishbone -< wbIn
  idC -< (mm, wbOut)

increaseBuswidth ::
  forall dom mode aw width power.
  (HiddenClockResetEnable dom, KnownNat aw, KnownNat width, KnownNat power, power <= aw) =>
  Circuit
    (Wishbone dom mode aw (BitVector (width * 8)))
    (Wishbone dom mode (aw - power) (BitVector (2 ^ power * width * 8)))
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
  (BitPack a, KnownNat aw, ShowX a) =>
  String ->
  Circuit (Wishbone dom mode aw a) (Wishbone dom mode aw a)
trace msg = Circuit (unbundle . fmap go . bundle)
 where
  go (m2s, s2m)
    | m2s.busCycle && m2s.strobe =
        (Debug.trace ([i| #{msg} M2S: #{showX m2s}, S2M: #{showX s2m} |]) s2m, m2s)
    | otherwise = (s2m, m2s)

mapMm ::
  Circuit a b ->
  Circuit (ToConstBwd Mm.Mm, a) (ToConstBwd Mm.Mm, b)
mapMm circ = circuit $ \(mm, a) -> do
  b <- circ -< a
  idC -< (mm, b)

strictM2S ::
  forall aw wordSize a.
  (KnownNat aw, KnownNat wordSize) =>
  WishboneM2S aw wordSize a ->
  WishboneM2S aw wordSize a
strictM2S
  ( WishboneM2S
      !addr
      !writeData
      !busSelect
      !lock
      !busCycles
      !strobe
      !writeEnable
      !cycleTypeIdentifier
      !burstType
    ) =
    WishboneM2S
      addr
      writeData
      busSelect
      lock
      busCycles
      strobe
      writeEnable
      cycleTypeIdentifier
      burstType

-- | Strictness helper for WishboneS2M to prevent space leaks from lazy record updates
strictS2M :: WishboneS2M c -> WishboneS2M c
strictS2M (WishboneS2M !a !b !c !d !e) = WishboneS2M a b c d e
