-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE MultiWayIf #-}

module Protocols.Wishbone.Extra (
  delayWishbone,
  xpmCdcHandshakeWb,
  increaseBusWidth,
  decreaseBusWidth,
) where

import Clash.Cores.Xilinx.Xpm.Cdc.Extra (xpmCdcHandshakeDf)
import Clash.Prelude
import Data.Maybe (fromMaybe, isJust)
import Protocols
import Protocols.Wishbone

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

data HandshakeWbManagerState
  = -- | Reset state; waiting to forward first request.
    HwmInReset
  | -- | Ready to forward requests to subordinate.
    HwmForwarding
  | -- | Request forwarded; waiting for response.
    HwmWaitForResponse
  deriving (Generic, NFDataX, Show, ShowX)

data HandshakeWbSubordinateState nBytes
  = -- | Reset state; waiting to accept first request.
    HwsInReset
  | -- | Ready to accept requests; waiting for valid transfer.
    HwsForwarding
  | -- | Request received; forwarding response to manager.
    HwsResponding (WishboneS2M nBytes)
  deriving (Generic, NFDataX, Show, ShowX)

{- | CDC Wishbone bridge using XPM handshake primitives.

Transfers Wishbone transactions between two clock domains with request/response
handshaking. Supports up to 1024-bit payloads.

XXX: Does not preserve @busCycle@ and @lock@ signals. The subordinate side
sees them asserted only while there is a transfer (@strobe@).

XXX: Test is still in `bittide` because the tests depend on @wbStorage@.
-}
xpmCdcHandshakeWb ::
  forall mgr sub aw nBytes.
  ( KnownDomain mgr
  , KnownDomain sub
  , KnownNat aw
  , KnownNat nBytes
  , 1 <= nBytes
  ) =>
  Clock mgr ->
  Reset mgr ->
  Clock sub ->
  Reset sub ->
  Circuit
    (Wishbone mgr 'Standard aw nBytes)
    (Wishbone sub 'Standard aw nBytes)
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
  m2sBitSize = SNat @(BitSize (WishboneM2S aw nBytes))
  s2mBitSize = SNat @(BitSize (WishboneS2M nBytes))

  managerC ::
    Circuit
      ( Wishbone mgr 'Standard aw nBytes
      , Df mgr (WishboneS2M nBytes)
      )
      (Df mgr (WishboneM2S aw nBytes))
  managerC = Circuit $ \((wbIn, dfS2Mm), ackIn) -> do
    let
      (wbOut, ackOut, dfM2Sm) =
        withClockResetEnable clkM rstM enableGen
          $ mealyB goManager HwmInReset (wbIn, dfS2Mm, ackIn)

    ((wbOut, ackOut), dfM2Sm)

  goManager ::
    HandshakeWbManagerState ->
    ( WishboneM2S aw nBytes
    , Maybe (WishboneS2M nBytes)
    , Ack
    ) ->
    ( HandshakeWbManagerState
    , ( WishboneS2M nBytes
      , Ack
      , Maybe (WishboneM2S aw nBytes)
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
      (Df sub (WishboneM2S aw nBytes))
      ( Wishbone sub 'Standard aw nBytes
      , Df sub (WishboneS2M nBytes)
      )
  subordinateC = Circuit $ \(m2sIn, (s2mIn, ackIn)) -> do
    let
      (ackOut, m2sOut, s2mOut) =
        withClockResetEnable clkS rstS enableGen
          $ mealyB goSubordinate HwsInReset (m2sIn, s2mIn, ackIn)

    (ackOut, (m2sOut, s2mOut))

  goSubordinate ::
    HandshakeWbSubordinateState nBytes ->
    ( Maybe (WishboneM2S aw nBytes)
    , WishboneS2M nBytes
    , Ack
    ) ->
    ( HandshakeWbSubordinateState nBytes
    , ( Ack
      , WishboneM2S aw nBytes
      , Maybe (WishboneS2M nBytes)
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

{- | Converts a bus of `width` bytes wide to bus of `(2 ^ power * width) based on the lower
`power` bits of the address. For example, with `power = 1`, a 32-bit bus becomes a 64-bit bus,
where even addresses access the lower 32 bits and odd addresses access the upper 32 bits.
-}
increaseBusWidth ::
  forall dom mode aw width power.
  (HiddenClockResetEnable dom, KnownNat aw, KnownNat width, KnownNat power, power <= aw) =>
  SNat power ->
  Circuit
    (Wishbone dom mode aw width)
    (Wishbone dom mode (aw - power) (2 ^ power * width))
increaseBusWidth SNat = Circuit (unbundle . fmap go . bundle)
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
    bitShift = natToNum @width * fromIntegral addrLsbs -- `shiftL` takes an `Int`
    (addrMsbs, addrLsbs :: BitVector power) = bitCoerce (m2sLeft.addr)
    newWriteData = pack (repeat m2sLeft.writeData)
    newBusSelect = shiftL (resize m2sLeft.busSelect) bitShift
    newReadData = resize (shiftR (s2mRight.readData) (8 * bitShift))

data DecreaseBusState power width
  = DecreaseBusState
  { addrLsbs :: BitVector power
  -- ^ Track the lower bits of the address to know where we are in the wider bus transaction.
  , dataReg :: BitVector (2 ^ power * width * 8)
  -- ^ Register to hold the read/write data as we shift it in/out over multiple cycles.
  , maskReg :: BitVector (2 ^ power * width)
  -- ^ Register to hold the bus select mask as we shift it out over multiple cycles.
  }
  deriving (Generic, NFDataX, Show, ShowX, Eq, BitPack)

{- | Converts a bus of `2 ^ power * width` bytes wide to bus of `width` bytes wide based on
the `busSelect` mask. It uses internal state to keep start at the lowest non-zero byte lane and
then perform operations on consecutive byte lanes until the entire transaction is complete.
-}
decreaseBusWidth ::
  forall dom mode aw width power.
  (HiddenClockResetEnable dom, KnownNat aw, KnownNat width, KnownNat power, power <= aw, 1 <= power) =>
  SNat power ->
  Circuit
    (Wishbone dom mode (aw - power) (2 ^ power * width))
    (Wishbone dom mode aw width)
decreaseBusWidth SNat = Circuit (unbundle . mealy go initState . bundle)
 where
  initState :: DecreaseBusState power width
  initState = DecreaseBusState 0 0 0
  go state (m2sLeft, s2mRight) = (newState1, (s2mLeft, m2sRight))
   where
    storedValid = state.maskReg /= 0
    masterActive = m2sLeft.busCycle && m2sLeft.strobe
    firstCycle = masterActive && not storedValid

    nextLsbs
      | m2sLeft.writeEnable = satSucc SatZero addrLsbs
      | otherwise = satPred SatZero addrLsbs

    nextDataReg
      | firstCycle && m2sLeft.writeEnable =
          resize (shiftR m2sLeft.writeData (natToNum @(width * 8)))
      | m2sLeft.writeEnable =
          state.dataReg `shiftR` (natToNum @(width * 8))
      | otherwise =
          resize (state.dataReg ++# s2mRight.readData)

    nextMaskReg
      | firstCycle = resize (shiftR m2sLeft.busSelect (natToNum @width))
      | otherwise = state.maskReg `shiftR` (natToNum @width)

    lastSubTransaction = nextMaskReg == 0
    transactionDone = lastSubTransaction || s2mRight.err || s2mRight.retry

    newState1
      | masterActive && hasTerminateFlag s2mRight && transactionDone = initState
      | masterActive && hasTerminateFlag s2mRight =
          DecreaseBusState nextLsbs nextDataReg nextMaskReg
      | masterActive = state
      | otherwise = initState

    -- On the first cycle of a transaction, we propagate the lower portion of all relevant fields
    -- to the rhs and store the rest in our state.

    -- wrap a vector of indices into Maybe's based on the bus select, then find the left most (highest) index.
    -- lastAddress :: Maybe (BitVector power)
    combine (bv :: BitVector width) idx
      | bv == 0 = Nothing
      | otherwise = Just (resize $ pack idx)

    lastAddress = fold (<|>) (zipWith combine (unpack m2sLeft.busSelect) (reverse indicesI) :< Nothing)

    newAddress = m2sLeft.addr ++# addrLsbs
    addrLsbs
      | firstCycle && not m2sLeft.writeEnable = fromMaybe 0 lastAddress
      | otherwise = state.addrLsbs

    newWriteData
      | storedValid = resize state.dataReg
      | otherwise = resize m2sLeft.writeData

    newBusSelect
      | storedValid = resize state.maskReg
      | otherwise = resize m2sLeft.busSelect

    m2sRight =
      m2sLeft
        { addr = newAddress
        , writeData = newWriteData
        , busSelect = newBusSelect
        }

    newReadData = resize (state.dataReg ++# s2mRight.readData)
    s2mLeft
      | masterActive && hasTerminateFlag s2mRight && transactionDone =
          s2mRight{readData = newReadData}
      | otherwise = emptyWishboneS2M
