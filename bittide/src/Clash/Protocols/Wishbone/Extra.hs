-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Clash.Protocols.Wishbone.Extra (xpmCdcHandshakeWb) where

import Clash.Prelude
import Protocols
import Protocols.Wishbone

import Clash.Cores.Xilinx.Xpm.Cdc.Extra (xpmCdcHandshakeDf)
import Data.Maybe (fromMaybe, isJust)

data HandshakeWbManagerState
  = -- | Reset state; waiting to forward first request.
    HwmInReset
  | -- | Ready to forward requests to subordinate.
    HwmForwarding
  | -- | Request forwarded; waiting for response.
    HwmWaitForResponse
  deriving (Generic, NFDataX, Show, ShowX)

data HandshakeWbSubordinateState a
  = -- | Reset state; waiting to accept first request.
    HwsInReset
  | -- | Ready to accept requests; waiting for valid transfer.
    HwsForwarding
  | -- | Request received; forwarding response to manager.
    HwsResponding (WishboneS2M a)
  deriving (Generic, NFDataX, Show, ShowX)

{- | CDC Wishbone bridge using XPM handshake primitives.

Transfers Wishbone transactions between two clock domains with request/response
handshaking. Supports up to 1024-bit payloads.

XXX: Does not preserve @busCycle@ and @lock@ signals. The subordinate side
sees them asserted only while there is a transfer (@strobe@).

XXX: I want to add this to @bittide-extra@, but the tests depend on @wbStorage@.
-}
xpmCdcHandshakeWb ::
  forall mgr sub aw a.
  ( KnownDomain mgr
  , KnownDomain sub
  , KnownNat aw
  , NFDataX a
  , ShowX a
  , BitPack a
  ) =>
  Clock mgr ->
  Reset mgr ->
  Clock sub ->
  Reset sub ->
  Circuit
    (Wishbone mgr 'Standard aw a)
    (Wishbone sub 'Standard aw a)
xpmCdcHandshakeWb clkM rstM clkS rstS =
  case (m2sBitSize `compareSNat` SNat @1024, s2mBitSize `compareSNat` SNat @1024) of
    (SNatLE, SNatLE) ->
      -- The wishbone CDC handshake implementation is based on two independent 'Df'
      -- streams. The idea being that we need to transfer a request over
      -- (WishboneM2S) and then wait for the response (WishboneS2M).
      --
      -- XXX: The "manager" in this equation is the LHS of the top circuit and
      --      the "subordinate" is the RHS. This is slightly confusing, because
      --      both of these are managers/subordinates in their own right when
      --      dealing with their own Wishbone/Df interfaces.
      --
      -- XXX: This function would be greatly helped by the existence of a
      --      handshake variant that has return data. Maybe we should build our
      --      own version at some point?
      circuit $ \wbIn -> do
        dfM2Sm <- managerC -< (wbIn, dfS2Mm)
        dfM2Ss <- xpmCdcHandshakeDf clkM rstM clkS rstS -< dfM2Sm
        dfS2Mm <- xpmCdcHandshakeDf clkS rstS clkM rstM -< dfS2Ms
        (wbOut, dfS2Ms) <- subordinateC -< dfM2Ss
        idC -< wbOut
    _ ->
      -- Realistically we only ship over CPU buses, which are much smaller than
      -- 1024 bytes, so this should never happen. Carrying around this constraint
      -- is very annoying, so we just error out here. But also, we should
      -- reimplement the Xilinx IP because this is INCREDIBLY dumb.
      clashCompileError "xpmCdcHandshakeWb: BitSize out of range (1-1024 bits)"
 where
  m2sBitSize = SNat @(BitSize (WishboneM2S aw (BitSize a `DivRU` 8) a))
  s2mBitSize = SNat @(BitSize (WishboneS2M a))

  managerC ::
    Circuit
      ( Wishbone mgr 'Standard aw a
      , Df mgr (WishboneS2M a)
      )
      (Df mgr (WishboneM2S aw (BitSize a `DivRU` 8) a))
  managerC = Circuit $ \((wbIn, dfS2Mm), ackIn) -> do
    let
      (wbOut, ackOut, dfM2Sm) =
        withClockResetEnable clkM rstM enableGen
          $ mealyB goManager HwmInReset (wbIn, dfS2Mm, ackIn)

    ((wbOut, ackOut), dfM2Sm)

  goManager ::
    HandshakeWbManagerState ->
    ( WishboneM2S aw (Div (BitSize a + 7) 8) a
    , Maybe (WishboneS2M a)
    , Ack
    ) ->
    ( HandshakeWbManagerState
    , ( WishboneS2M a
      , Ack
      , Maybe (WishboneM2S aw (Div (BitSize a + 7) 8) a)
      )
    )
  goManager HwmInReset (_m2sIn, _s2mIn, _ackIn) =
    (HwmForwarding, (emptyWishboneS2M, Ack False, Nothing))
  goManager HwmForwarding (m2sIn, _s2mIn, Ack ackIn) =
    (state, (emptyWishboneS2M, Ack False, m2sOut))
   where
    m2sOut
      | m2sIn.busCycle && m2sIn.strobe = Just m2sIn
      | otherwise = Nothing
    state
      | isJust m2sOut && ackIn = HwmWaitForResponse
      | otherwise = HwmForwarding
  goManager HwmWaitForResponse (_m2sIn, s2mIn, _ackIn) =
    (state, (fromMaybe emptyWishboneS2M s2mIn, Ack True, Nothing))
   where
    -- Note that at this point, whenever 's2mIn' is a 'Just', it will have a
    -- terminate flag set (see implementation of 'subordinateC').
    state
      | isJust s2mIn = HwmForwarding
      | otherwise = HwmWaitForResponse

  subordinateC ::
    Circuit
      (Df sub (WishboneM2S aw (BitSize a `DivRU` 8) a))
      ( Wishbone sub 'Standard aw a
      , Df sub (WishboneS2M a)
      )
  subordinateC = Circuit $ \(m2sIn, (s2mIn, ackIn)) -> do
    let
      (ackOut, m2sOut, s2mOut) =
        withClockResetEnable clkS rstS enableGen
          $ mealyB goSubordinate HwsInReset (m2sIn, s2mIn, ackIn)

    (ackOut, (m2sOut, s2mOut))

  goSubordinate ::
    HandshakeWbSubordinateState a ->
    ( Maybe (WishboneM2S aw (Div (BitSize a + 7) 8) a)
    , WishboneS2M a
    , Ack
    ) ->
    ( HandshakeWbSubordinateState a
    , ( Ack
      , WishboneM2S aw (Div (BitSize a + 7) 8) a
      , Maybe (WishboneS2M a)
      )
    )
  goSubordinate HwsInReset (_m2sIn, _s2mIn, _ackIn) =
    (HwsForwarding, (Ack False, emptyWishboneM2S, Nothing))
  goSubordinate HwsForwarding (Nothing, _s2mIn, _ackIn) =
    (HwsForwarding, (Ack False, emptyWishboneM2S, Nothing))
  goSubordinate HwsForwarding (Just m2sIn, s2mIn, Ack _ackIn) =
    (state, (Ack terminate, m2sOut, Nothing))
   where
    m2sOut = m2sIn
    terminate = hasTerminateFlag s2mIn
    state
      | terminate = HwsResponding s2mIn
      | otherwise = HwsForwarding
  goSubordinate (HwsResponding s2mIn) (_m2sIn, _s2mIn, Ack ackIn) =
    (state, (Ack False, emptyWishboneM2S, Just s2mIn))
   where
    state
      | ackIn = HwsForwarding
      | otherwise = HwsResponding s2mIn
