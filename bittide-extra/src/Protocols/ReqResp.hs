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

import Data.Bifunctor (Bifunctor (..))
import Data.Kind (Type)
import Data.Maybe
import Protocols
import Protocols.BiDf (BiDf)
import qualified Protocols.BiDf as BiDf
import Protocols.Idle
import Prelude as P

import qualified Protocols.Df as Df

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

dropResponse :: resp -> Circuit (ReqResp dom req resp) (ReqResp dom req ())
dropResponse resp = applyC id (fmap $ fmap $ const resp)

{- | Force a @Nothing@ on the backward channel and @Nothing@ on the forward
channel if reset is asserted.
-}
forceResetSanity ::
  forall dom req resp.
  (C.HiddenReset dom) =>
  Circuit (ReqResp dom req resp) (ReqResp dom req resp)
forceResetSanity = forceResetSanityGeneric

-- | Convert a `ReqResp` protocol to two `Df` streams, one for requests and one for responses.
toDfs ::
  forall dom req resp.
  (C.HiddenClockResetEnable dom) =>
  Circuit (ReqResp dom req resp, Df dom resp) (Df dom req)
toDfs = Circuit (first C.unbundle . C.unbundle . C.mealy go False . C.bundle . first C.bundle)
 where
  go accepted0 ~(~(reqLeft, resp), Ack reqRightAck) = (accepted1, ((resp, respAck), reqRight))
   where
    respAck = Ack True
    reqRight
      | accepted0 = Nothing
      | otherwise = reqLeft
    accepted1
      | isNothing reqLeft = False -- no request
      | isJust resp = False -- Receiving a response clears the state
      | otherwise = reqRightAck -- A request for which we have not received a response yet

-- | Convert two `Df` streams for requests and responses into a `ReqResp` protocol.
fromDfs ::
  forall dom req resp.
  (C.HiddenClockResetEnable dom) =>
  Circuit (Df dom req) (ReqResp dom req resp, Df dom resp)
fromDfs = Circuit (second C.unbundle . C.unbundle . fmap go . C.bundle . second C.bundle)
 where
  go ~(req, ~(resp, reqAck)) = (reqAck, (req, resp))

-- | Convert a `ReqResp` protocol to a `BiDf` protocol through `toDfs` and `BiDf.fromDfs`.
toBiDf ::
  forall dom req resp.
  (C.HiddenClockResetEnable dom) =>
  Circuit (ReqResp dom req resp) (BiDf dom req resp)
toBiDf = circuit $ \reqresp -> do
  request <- toDfs -< (reqresp, response)
  (biDf, response) <- BiDf.fromDfs -< request
  idC -< biDf

-- | Convert a `BiDf` protocol to a `ReqResp` protocol through `fromDfs` and `BiDf.toDfs`.
fromBiDf ::
  forall dom req resp.
  (C.HiddenClockResetEnable dom) =>
  Circuit (BiDf dom req resp) (ReqResp dom req resp)
fromBiDf = circuit $ \biDf -> do
  request <- BiDf.toDfs -< (biDf, response)
  (reqresp, response) <- fromDfs -< request
  idC -< reqresp

-- | Convert a `ReqResp` protocol where the response type is `()` to a `Df` stream of requests.
requests ::
  forall dom req.
  (C.KnownDomain dom) =>
  Circuit (ReqResp dom req ()) (Df dom req)
requests = Circuit (C.unbundle . fmap go . C.bundle)
 where
  go ~(request, Ack ack) = (if ack then Just () else Nothing, request)

data PrefetchState req = Empty | OfferingRequest req | WaitingForResponse req
  deriving (C.Generic, C.NFDataX, C.BitPack, C.Show, C.ShowX, C.Eq)

{- | Terminate a `ReqResp` protocol by turning the requests into responses based on a supplied
function.
-}
loopback ::
  (req -> resp) ->
  Circuit (ReqResp dom req resp) ()
loopback f = Circuit go
 where
  go (req, _) = (fmap (fmap f) req, ())

{- | Component that handles invalidating a prefetched requests.
Can be used to invalidate any request, or a specific request without having to
worry about backpressure.
-}
prefetchDrop ::
  forall dom req.
  (C.HiddenClockResetEnable dom, C.NFDataX req) =>
  C.Signal dom (Maybe (Maybe req)) ->
  Circuit () (Df dom (Maybe req))
prefetchDrop req = circuit $ do
  Df.compressor Nothing (\s i -> (i, Just s)) -< Fwd req

-- | Component to invalidate any prefetched request without having to worry about backpressure.
prefetchDropAny ::
  forall dom req.
  (C.HiddenClockResetEnable dom, C.NFDataX req) =>
  C.Signal dom Bool ->
  Circuit () (Df dom (Maybe req))
prefetchDropAny dropReq = prefetchDrop (C.mux dropReq (pure (Just Nothing)) (pure Nothing))

-- | Component to invalidate a specific prefetched request without having to worry about backpressure.
prefetchDropSpecific ::
  forall dom req.
  ( C.HiddenClockResetEnable dom
  , Eq req
  , C.NFDataX req
  ) =>
  C.Signal dom (Maybe req) ->
  Circuit () (Df dom (Maybe req))
prefetchDropSpecific req = prefetchDrop (fmap (fmap Just) req)

{- | Speculatively prefetches the successor of the last request for requests that are not `maxBound`.

When there is no pending user request, this circuit speculatively issues a request
for @succ lastRequest@ to hide memory latency. When a real user request arrives,
it checks if the prefetched request matches - if so, it's a cache hit and the
response is immediately available. Otherwise, it drops the prefetch and issues
the new request.

The second `Df` input can be used to clear the cached request. If the payload of the second `Df`
is `Just req`, the circuit will drop the cached request if it matches `req`. If the payload
is `Nothing`, the circuit will drop the cached request unconditionally.

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
  , Bounded req
  , C.NFDataX req
  , C.NFDataX resp
  ) =>
  --      (((req, ack), mreq), (ack, resp)) -> (((ack, resp), ack), (req, ack))
  Circuit (ReqResp dom req resp, Df dom (Maybe req)) (BiDf dom req resp)
prefetch =
  Circuit
    ( bimap (first C.unbundle . C.unbundle) C.unbundle
        . C.unbundle
        . C.mealy go Empty
        . C.bundle
        . bimap (C.bundle . first C.bundle) C.bundle
    )
 where
  go storedReq0 ~(~(reqLeft, invalidate), ~(Ack reqRightAck, respRight)) =
    (storedReq1, ((respLeft, Ack invalidateDone), (reqRight, Ack respRightAck)))
   where
    -- Check if we have a cache hit: user request matches prefetched request
    emptyReg = storedReq0 == Empty
    (clear, invalidateDone) = case (invalidate, storedReq0) of
      (Just _, Empty) -> (False, True) -- No request to invalidate
      (Just Nothing, WaitingForResponse _) -> (isJust reqLeft, True) -- Unconditional invalidation
      (Just (Just reqA), OfferingRequest reqB) -> (False, reqA /= reqB) -- Prematurely ack invalidation if the prefetched request is different from the invalidation request
      (Just (Just reqA), WaitingForResponse reqB) -> (isJust reqLeft && reqA == reqB, True)
      _ -> (False, False)

    (hit, miss) = case (storedReq0, reqLeft) of
      (WaitingForResponse stored, Just req) | isJust respRight -> (req == stored, req /= stored)
      _ -> (False, False)

    requestAccepted = isJust reqRight && reqRightAck
    responseAccepted = isJust respLeft

    respRightAck = if miss then True else responseAccepted -- Discard prefetched response on miss

    -- Determine what request to issue
    reqRight = case storedReq0 of
      Empty -> reqLeft -- No prefetched request, just forward user request
      (OfferingRequest stored) -> Just stored -- Keep offering prefetched request until it's accepted
      (WaitingForResponse stored) | responseAccepted && stored /= maxBound -> Just $ succ stored
      -- (WaitingForResponse _) | miss             -> reqLeft -- Cache miss, drop prefetched request and forward user request
      _ -> Nothing

    -- Output response handling
    respLeft
      | clear = Nothing -- Dont forward invalidated response
      | hit = respRight -- Cache hit: forward response
      | otherwise = Nothing

    -- Update prefetched request
    storedReq1
      | clear = Empty -- Invalidate prefetched request
      | requestAccepted = WaitingForResponse (C.fromJustX reqRight) -- Some request has been accepted
      | responseAccepted = maybe Empty OfferingRequest reqRight -- Cache hit, but request not accepted. Keep offering the same request until it is accepted.
      | emptyReg = maybe Empty OfferingRequest reqLeft -- No pending request, wait for user request
      | miss = Empty -- Cache miss, drop stored request
      | otherwise = storedReq0 -- Keep current request
