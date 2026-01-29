-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- |
Simplest possible protocol for request-response communication.

The forward channel channel has type @Signal dom (Maybe req)@ and is used to send requests.
The backward channel has type @Signal dom (Maybe resp)@ and is used to send responses.

The protocol must obey the following rules:
* When the forward channel is @Just a@, it must not change until the transaction is completed.
* The forward channel can not depend on the backward channel.
* When the forward channel is @Nothing@, the backward channel may be undefined.

This protocol can not be pipelined, for pipelined request-response communication see `Protocols.BiDf`.
-}
module Protocols.ReqResp where

import qualified Clash.Prelude as C

import Clash.Explicit.Prelude (NFDataX)
import Data.Bifunctor (Bifunctor (..))
import Data.Kind (Type)
import Data.Maybe
import Protocols
import Protocols.Idle
import Prelude as P

{- |
Simplest possible protocol for request-response communication.

The forward channel channel has type @Signal dom (Maybe req)@ and is used to send requests.
The backward channel has type @Signal dom (Maybe resp)@ and is used to send responses.

The protocol must obey the following rules:
* When the forward channel is @Just a@, it must not change until the transaction is completed.
* The forward channel can not depend on the backward channel.
* When the forward channel is @Nothing@, the backward channel may be undefined.
-}
data ReqResp (dom :: C.Domain) (req :: Type) (resp :: Type)

instance Protocol (ReqResp dom req resp) where
  -- \| Forward channel for ReqResp protocol:
  type Fwd (ReqResp dom req resp) = C.Signal dom (Maybe req)

  -- \| Backward channel for ReqResp protocol:
  type Bwd (ReqResp dom req resp) = C.Signal dom (Maybe resp)

instance IdleCircuit (ReqResp dom req resp) where
  idleFwd _ = pure Nothing
  idleBwd _ = pure Nothing

leftToMaybe :: Either a b -> Maybe a
leftToMaybe (Left x) = Just x
leftToMaybe (Right _) = Nothing
rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _) = Nothing
rightToMaybe (Right y) = Just y

partitionEithers ::
  forall dom a b c. Circuit (ReqResp dom (Either a b) c) (ReqResp dom a c, ReqResp dom b c)
partitionEithers = Circuit goS
 where
  goS (eitherFwd, (leftBwd, rightBwd)) = (eitherBwd, (leftFwd, rightFwd))
   where
    leftFwd = fmap (>>= leftToMaybe) eitherFwd
    rightFwd = fmap (>>= rightToMaybe) eitherFwd

    eitherBwd = selectBwd <$> eitherFwd <*> leftBwd <*> rightBwd
  selectBwd (Just (Left _)) leftBwd _ = leftBwd
  selectBwd (Just (Right _)) _ rightBwd = rightBwd
  selectBwd Nothing _ _ = Nothing

fromBlockramWithMask ::
  (C.HiddenClockResetEnable dom, Num addr, C.KnownNat words) =>
  ( C.Signal dom addr ->
    C.Signal dom (Maybe (addr, C.BitVector (words C.* 8))) ->
    C.Signal dom (C.BitVector words) ->
    C.Signal dom (C.BitVector (words C.* 8))
  ) ->
  Circuit
    ( ReqResp dom addr (C.BitVector (words C.* 8))
    , ReqResp dom (addr, C.BitVector words, C.BitVector (words C.* 8)) ()
    )
    ()
fromBlockramWithMask primitive = Circuit go
 where
  writeBwd = pure $ Just ()
  go ((readFwd, writeFwd), _) = ((readBwd, writeBwd), ())
   where
    -- Separate the write data and byte enables
    writeData = fmap (>>= \(addr, _mask, dat) -> Just (addr, dat)) writeFwd
    byteEnables = fmap (\case Just (_, mask, _) -> mask; Nothing -> 0) writeFwd

    readData = primitive (fromMaybe 0 <$> readFwd) writeData byteEnables

    -- Reading takes 1 cycle so we run at half speed
    readValid = C.register False (fmap isJust readFwd C..&&. fmap not readValid)
    readBwd = liftA2 (\v d -> if v then Just d else Nothing) readValid readData

fromBlockRam ::
  (C.HiddenClockResetEnable dom, Num addr) =>
  (C.Signal dom addr -> C.Signal dom (Maybe (addr, a)) -> C.Signal dom a) ->
  Circuit (ReqResp dom addr a, ReqResp dom (addr, a) ()) ()
fromBlockRam primitive = Circuit go
 where
  writeBwd = pure $ Just ()
  go ((readFwd, writeFwd), _) = ((readBwd, writeBwd), ())
   where
    readData = primitive (fromMaybe 0 <$> readFwd) writeFwd

    -- Reading takes 1 cycle so we run at half speed
    readValid = C.register False (fmap isJust readFwd C..&&. fmap not readValid)
    readBwd = liftA2 (\v d -> if v then Just d else Nothing) readValid readData

dropResponseData :: resp -> Circuit (ReqResp dom req resp) (ReqResp dom req ())
dropResponseData resp = applyC id (fmap $ fmap $ const resp)

generate ::
  forall dom req resp.
  (C.HiddenClockResetEnable dom, NFDataX req, NFDataX resp) =>
  (req -> req) ->
  req ->
  Circuit () (ReqResp dom req resp, Df dom resp)
generate f s0 = Circuit (((),) . C.unbundle . C.mealy go (s0, Nothing) . C.bundle . snd)
 where
  go (state0, respStored0) (respIn, Ack respAck) = ((state1, respStored1), (reqOut, respOut))
   where
    stalled = isJust respStored0 && not respAck
    respStored1
      | stalled = respStored0
      | otherwise = respIn

    reqOut = if stalled then Nothing else Just state0
    respOut = respStored0
    state1
      | stalled = state0
      | isNothing respIn = state0
      | otherwise = f state0

{- | Force a @Nothing@ on the backward channel and @Nothing@ on the forward
channel if reset is asserted.
-}
forceResetSanity ::
  forall dom req resp.
  (C.HiddenReset dom) =>
  Circuit (ReqResp dom req resp) (ReqResp dom req resp)
forceResetSanity = forceResetSanityGeneric

toDf ::
  forall dom req resp.
  (C.HiddenClockResetEnable dom) =>
  Circuit (ReqResp dom req resp, Df dom resp) (Df dom req)
toDf = Circuit (first C.unbundle . C.unbundle . C.mealy go False . C.bundle . first C.bundle)
 where
  go accepted0 ((reqLeft, resp), Ack reqRightAck) = (accepted1, ((resp, respAck), reqRight))
   where
    respAck = Ack True
    reqRight
      | accepted0 = Nothing
      | otherwise = reqLeft
    accepted1
      | isNothing reqLeft = False -- no request
      | isJust resp = False -- Receiving a response clears the state
      | otherwise = reqRightAck -- A request for which we have not received a response yet

{- | Like 'toDf' but speculatively prefetches the successor of the last request.
When there is no pending user request, this circuit speculatively issues a request
for @succ lastRequest@ to hide memory latency. When a real user request arrives,
it checks if the prefetched request matches - if so, it's a cache hit and the
response is immediately available. Otherwise, it drops the prefetch and issues
the new request.

State: (prefetchedReq, prefetchedResp, lastUserReq)
- prefetchedReq: the request we speculatively sent
- prefetchedResp: the response we received (if any)
- lastUserReq: the last user request, used to derive next prefetch
-}
prefetch ::
  forall dom req resp.
  ( C.HiddenClockResetEnable dom
  , Eq req
  , Enum req
  , NFDataX req
  , NFDataX resp
  ) =>
  Circuit (ReqResp dom req resp, Df dom resp) (Df dom req)
prefetch =
  Circuit (first C.unbundle . C.unbundle . C.mealy go Nothing . C.bundle . first C.bundle)
 where
  go storedReq0 ((reqLeft, resp), Ack reqRightAck) =
    (storedReq1, ((respOut, respAck), reqRight))
   where
    respAck = Ack True

    -- Check if we have a cache hit: user request matches prefetched request

    empty = isNothing storedReq0
    hit = reqLeft == storedReq0 && isJust resp
    miss = isJust reqLeft && not hit && isJust resp
    nextReq = fmap succ storedReq0

    -- Determine what request to issue
    reqRight
      | empty = reqLeft -- Empty cache: just forward user request
      | hit = nextReq -- Cache hit: prefetch next
      | miss = reqLeft -- Cache miss: forward user request
      | otherwise = Nothing

    -- Output response handling
    respOut
      | hit = resp -- Cache hit: forward response
      | otherwise = Nothing

    -- Update prefetched request
    storedReq1
      | empty && reqRightAck = reqLeft -- new request accepted
      | hit && reqRightAck = nextReq -- Prefetch accepted
      | miss && reqRightAck = Nothing -- Cache miss: replace prefetch with accepted user req
      | otherwise = storedReq0 -- Keep current prefetch
