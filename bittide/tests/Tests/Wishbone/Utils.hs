-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- TODO: Use clash-protocols's utilities / strip out bittide specific stuff

module Tests.Wishbone.Utils (
  forwardPressure,
  backPressure,
  driver,
  monitor,
  sink,
  raw,

  -- * Helper functions
  wishboneRequestToM2S,
  pairToWishboneRequestResponse,
  genWishboneRequest,
) where

import Clash.Prelude
import Protocols

import Protocols.Wishbone

import Bittide.SharedTypes (Byte)
import Bittide.Wishbone (WishboneRequest (..), WishboneResponse (..))
import Hedgehog (Gen)
import Tests.Shared (genDefinedBitVector)

import qualified Hedgehog.Gen as Gen

{- | Delay a wishbone request by /n/ cycles. If the given list is shorter than
the number of wishbone transactions seen, later transactions are passed
through without delays.
-}
forwardPressure ::
  ( HiddenClockResetEnable dom
  , KnownNat addrW
  , KnownNat nBytes
  ) =>
  [Int] ->
  Circuit
    (Wishbone dom 'Standard addrW nBytes)
    (Wishbone dom 'Standard addrW nBytes)
forwardPressure delays = Circuit (mealyB go delays)
 where
  go [] (m2s, s2m) = ([], (s2m, m2s))
  go (d : ds) ~(m2s, s2m)
    | m2s.busCycle && m2s.strobe && d > 0 =
        (d - 1 : ds, (emptyWishboneS2M, emptyWishboneM2S))
    | m2s.busCycle && m2s.strobe =
        (ds, (s2m, m2s))
    | otherwise = (d : ds, (s2m, m2s))

{- | Delay a wishbone request by stalling for /n/ cycles. If the given list is
shorter than the number of wishbone transactions seen, later transactions are
passed through without delays.
-}
backPressure ::
  ( HiddenClockResetEnable dom
  , KnownNat addrW
  , KnownNat nBytes
  ) =>
  [Int] ->
  Circuit
    (Wishbone dom 'Standard addrW nBytes)
    (Wishbone dom 'Standard addrW nBytes)
backPressure delays = Circuit (mealyB go delays)
 where
  go [] (m2s, s2m) = ([], (s2m, m2s))
  go (d : ds) ~(m2s, s2m)
    | m2s.busCycle && m2s.strobe && d > 0 =
        (d - 1 : ds, (emptyWishboneS2M, m2s))
    | m2s.busCycle && m2s.strobe =
        (ds, (s2m, m2s))
    | otherwise = (d : ds, (s2m, m2s))

-- | Driver that sends a sequence of Wishbone requests and waits for acknowledgment
driver ::
  forall dom addrW n.
  ( HiddenClockResetEnable dom
  , KnownNat addrW
  , KnownNat n
  ) =>
  [WishboneRequest addrW n] ->
  Circuit () (Wishbone dom 'Standard addrW n)
driver requests = Circuit (((),) . mealy go requests . snd)
 where
  go ::
    [WishboneRequest addrW n] ->
    WishboneS2M n ->
    ( [WishboneRequest addrW n]
    , WishboneM2S addrW n
    )
  go [] _ = ([], emptyWishboneM2S @addrW)
  go (req : reqs) s2m = (newReqs, wishboneRequestToM2S req)
   where
    newReqs
      | hasTerminateFlag s2m = reqs
      | otherwise = (req : reqs)

-- | Sink that acknowledges all Wishbone requests
sink ::
  forall dom addrW n.
  ( HiddenClockResetEnable dom
  , KnownNat addrW
  , KnownNat n
  ) =>
  Circuit (Wishbone dom 'Standard addrW n) ()
sink = Circuit (\_ -> (pure emptyWishboneS2M{acknowledge = True}, ()))

{- | Monitor that exposes the Wishbone requests and responses whenever there is
a valid transaction.
-}
monitor ::
  forall dom addrW n.
  (KnownNat n, KnownNat addrW) =>
  Circuit
    (Wishbone dom 'Standard addrW n)
    ( Wishbone dom 'Standard addrW n
    , CSignal dom (Maybe (WishboneRequest addrW n, WishboneResponse n))
    )
monitor = Circuit go0
 where
  go0 (m2s0, (s2m0, _)) = (s2m1, (m2s1, resp))
   where
    (s2m1, m2s1, resp) = unbundle (fmap go1 (bundle (m2s0, s2m0)))

  go1 (m2s, s2m) = (s2m, m2s, pairToWishboneRequestResponse m2s s2m)

-- | Expose the raw Wishbone M2S and S2M signals alongside the Wishbone interface
raw ::
  forall dom addrW n.
  (KnownNat n, KnownNat addrW) =>
  Circuit
    (Wishbone dom 'Standard addrW n)
    ( Wishbone dom 'Standard addrW n
    , CSignal dom (WishboneM2S addrW n)
    , CSignal dom (WishboneS2M n)
    )
raw = Circuit go
 where
  go (m2s, (s2m, _, _)) = (s2m, (m2s, m2s, s2m))

-- | Transform a 'WishboneRequest' into 'WishboneM2S'
wishboneRequestToM2S ::
  forall addrW n.
  (KnownNat n, KnownNat addrW) =>
  WishboneRequest addrW n ->
  WishboneM2S addrW n
wishboneRequestToM2S = \case
  ReadRequest addr sel ->
    (emptyWishboneM2S @addrW)
      { busCycle = True
      , strobe = True
      , addr = addr
      , busSelect = sel
      }
  WriteRequest addr sel dat ->
    (emptyWishboneM2S @addrW @n)
      { busCycle = True
      , strobe = True
      , writeEnable = True
      , addr = addr
      , busSelect = sel
      , writeData = pack dat
      }

{- | Transform a pair of 'WishboneM2S' and 'WishboneS2M' into a
'WishboneRequest' and 'WishboneResponse', if there was a valid transaction.
-}
pairToWishboneRequestResponse ::
  forall addrW n.
  (KnownNat n, KnownNat addrW) =>
  WishboneM2S addrW n ->
  WishboneS2M n ->
  Maybe (WishboneRequest addrW n, WishboneResponse n)
pairToWishboneRequestResponse m2s s2m
  | m2s.busCycle && m2s.strobe && hasTerminateFlag s2m =
      Just (req, resp)
  | otherwise = Nothing
 where
  req
    | m2s.writeEnable =
        WriteRequest m2s.addr m2s.busSelect (unpack m2s.writeData)
    | otherwise =
        ReadRequest m2s.addr m2s.busSelect

  resp
    | s2m.err && m2s.writeEnable = WriteError
    | s2m.err = ReadError
    | m2s.writeEnable = WriteSuccess
    | otherwise =
        ReadSuccess (mux (unpack m2s.busSelect) (map Just readDataVec) (repeat Nothing))

  readDataVec :: Vec n Byte
  readDataVec = unpack s2m.readData

genWishboneRequest ::
  (KnownNat addrW, KnownNat nBytes) =>
  Gen (WishboneRequest addrW nBytes)
genWishboneRequest =
  Gen.choice
    [ ReadRequest <$> genDefinedBitVector <*> genDefinedBitVector
    , WriteRequest
        <$> genDefinedBitVector
        <*> genDefinedBitVector
        <*> genDefinedBitVector
    ]
