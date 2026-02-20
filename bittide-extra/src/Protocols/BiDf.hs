-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

-- | Bi-directional request/response-style 'Df' channels.
module Protocols.BiDf (
  BiDf,

  -- * Conversion
  fromDfs,
  toDfs,
  prefetch,
  prefetch2,
  fromBiDf,
  toBiDf,

  -- * Trivial combinators
  void,
  loopback,

  -- * Mapping
  dimap,

  -- * Fan-in
  fanin,
) where

import Prelude ()

import Clash.Prelude
import Data.Bifunctor
import Data.Maybe (isJust, isNothing)
import Protocols
import qualified Protocols.Df as Df

{- | A 'Protocol' allowing requests to be passed downstream, with corresponding
responses being passed back upstream. Responses are provided in the order that
their corresponding requests were submitted.

*Correctness conditions*

 - The response channel must not produce a value before the request channel
   has produced a value. The response may be produced in the same cycle the
   request is acknowledged (but see the law about a combinational path
   below).

 - Each request must be paired with exactly one response.

 - Responses must be issued in the order that their corresponding requests arrived.

 - Both the request and response channels must obey usual 'Df' correctness
   conditions.

 - There must not be a combinational path from the request channel to the
   response channel.
-}
type BiDf dom req resp =
  (Df dom req, Reverse (Df dom resp))

-- | Convert a circuit of 'Df's to a 'BiDf' circuit.
toBiDf ::
  Circuit (Df dom req) (Df dom resp) ->
  Circuit (BiDf dom req resp) ()
toBiDf c = circuit $ \bidf -> do
  resp <- c -< req
  req <- toDfs -< (bidf, resp)
  idC -< ()

-- | Convert a 'BiDf' circuit to a circuit of 'Df's.
fromBiDf ::
  Circuit (BiDf dom req resp) () ->
  Circuit (Df dom req) (Df dom resp)
fromBiDf c = circuit $ \req -> do
  (biDf, resp) <- fromDfs -< req
  c -< biDf
  idC -< resp

-- | Convert a pair of a request and response 'Df`s into a 'BiDf'.
toDfs :: Circuit (BiDf dom req resp, Df dom resp) (Df dom req)
toDfs = fromSignals $ \(~((reqData, respAck), respData), reqAck) ->
  (((reqAck, respData), respAck), reqData)

-- | Convert a 'BiDf' into a pair of request and response 'Df`s.
fromDfs :: Circuit (Df dom req) (BiDf dom req resp, Df dom resp)
fromDfs = fromSignals $ \(reqData, ~((reqAck, respData), respAck)) ->
  (reqAck, ((reqData, respAck), respData))

{- | Like 'toDfs' but speculatively prefetches the successor of the last request.
When there is no pending user request, this circuit speculatively issues a request
for @succ lastRequest@ to hide memory latency. When a real user request arrives,
it checks if the prefetched request matches - if so, it's a cache hit and the
response is immediately available. Otherwise, it drops the prefetch and issues
the new request.
-}
prefetch ::
  forall dom req resp.
  ( HiddenClockResetEnable dom
  , Eq req
  , Enum req
  , NFDataX req
  , NFDataX resp
  ) =>
  Circuit (BiDf dom req resp, Df dom resp) (Df dom req)
prefetch = fromSignals go
 where
  go
    ( ~((reqData, respAckIn), respData)
      , reqAck
      ) =
      ( ((reqAck, respOut), respAckOut)
      , reqRight
      )
     where
      (respOut, reqRight, respAckOut) =
        unbundle (mealy step Nothing (bundle (reqData, respData, reqAck, respAckIn)))

  step storedReq0 (reqLeft, resp, Ack reqRightAck, Ack respAckIn) =
    (storedReq1, (respOut, reqRight, Ack respAckOut))
   where
    emptyReg = isNothing storedReq0
    hit = reqLeft == storedReq0 && isJust resp
    miss = isJust reqLeft && not hit && isJust resp
    nextReq = fmap succ storedReq0

    reqRight
      | emptyReg = reqLeft
      | hit && respAckIn = nextReq
      | hit = Nothing
      | miss = reqLeft
      | otherwise = Nothing

    respOut
      | hit = resp
      | otherwise = Nothing

    respAckOut
      | hit = respAckIn
      | miss = True
      | otherwise = respAckIn

    storedReq1
      | emptyReg && reqRightAck = reqLeft
      | hit && respAckIn && reqRightAck = nextReq
      | miss && reqRightAck = Nothing
      | otherwise = storedReq0

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
prefetch2 ::
  forall dom req resp.
  ( HiddenClockResetEnable dom
  , Eq req
  , Enum req
  , NFDataX req
  , NFDataX resp
  ) =>
  Circuit (BiDf dom req resp) (BiDf dom req resp)
prefetch2 =
  Circuit (bimap unbundle unbundle . unbundle . mealy go Nothing . bundle . bimap bundle bundle)
 where
  go storedReq0 ((reqLeft, respLeftAck), (Ack reqRightAck, respRight)) =
    (storedReq1, ((reqLeftAck, respLeft), (reqRight, respRightAck)))
   where
    respRightAck, reqLeftAck :: Ack
    respRightAck = respLeftAck
    reqLeftAck = respLeftAck

    -- Check if we have a cache hit: user request matches prefetched request

    emptyReg = isNothing storedReq0
    hit = reqLeft == storedReq0 && isJust respRight
    miss = isJust reqLeft && not hit && isJust respRight
    nextReq = fmap succ storedReq0

    -- Determine what request to issue
    reqRight
      | emptyReg = reqLeft -- Empty cache: just forward user request
      | hit = nextReq -- Cache hit: prefetch next
      | miss = reqLeft -- Cache miss: forward user request
      | otherwise = Nothing

    -- Output response handling
    respLeft
      | hit = respRight -- Cache hit: forward response
      | otherwise = Nothing

    -- Update prefetched request
    storedReq1
      | emptyReg && reqRightAck = reqLeft -- new request accepted
      | hit && reqRightAck = nextReq -- Prefetch accepted
      | miss && reqRightAck = Nothing -- Cache miss: replace prefetch with accepted user req
      | otherwise = storedReq0 -- Keep current prefetch

-- | Ignore all requests, never providing responses.
void :: (HiddenClockResetEnable dom) => Circuit (BiDf dom req resp') ()
void = circuit $ \biDf -> do
  req <- toDfs -< (biDf, resp)
  resp <- Df.empty -< ()
  Df.void -< req

-- | Return mapped requests as responses.
loopback ::
  (HiddenClockResetEnable dom, NFDataX req) =>
  (req -> resp) ->
  Circuit (BiDf dom req resp) ()
loopback f = circuit $ \biDf -> do
  req <- toDfs -< (biDf, resp)
  resp <- Df.map f <| Df.registerFwd -< req
  idC -< ()

-- | Map both requests and responses.
dimap ::
  (req -> req') ->
  (resp -> resp') ->
  Circuit (BiDf dom req resp') (BiDf dom req' resp)
dimap f g = circuit $ \biDf -> do
  req <- toDfs -< (biDf, resp')
  req' <- Df.map f -< req
  resp' <- Df.map g -< resp
  (biDf', resp) <- fromDfs -< req'
  idC -< biDf'

-- | Merge a number of 'BiDf's, preferring requests from the last channel.
fanin ::
  forall n dom req resp.
  ( KnownNat n
  , 1 <= n
  , NFDataX req
  , NFDataX resp
  , HiddenClockResetEnable dom
  ) =>
  Circuit (Vec n (BiDf dom req resp)) (BiDf dom req resp)
fanin = fromSignals $ \(upFwds, (reqAck, respData)) ->
  let reqDatas :: Vec n (Signal dom (Maybe req))
      reqDatas = map fst upFwds
      respAcks :: Vec n (Signal dom Ack)
      respAcks = map snd upFwds

      ((reqAcks, respAck), (respDatas, reqData)) =
        toSignals fanin' ((reqDatas, respData), (respAcks, reqAck))
   in (zip reqAcks respDatas, (reqData, respAck))
 where
  fanin' ::
    Circuit
      (Vec n (Df dom req), Df dom resp)
      (Vec n (Df dom resp), Df dom req)
  fanin' = circuit $ \(reqs, resp) -> do
    [fwd0, fwd1] <-
      Df.fanout
        <| Df.roundrobinCollect @n Df.Parallel
        <| repeatWithIndexC (\i -> Df.map (\x -> (i, x)))
        -< reqs

    activeN <- Df.map fst -< fwd1
    resps <- Df.route <| Df.zip -< (activeN, resp)
    req <- Df.map snd -< fwd0
    idC -< (resps, req)

{- | Copy a circuit /n/ times, providing access to the index of each replica.
If looking for a circuit that turns a single channel into multiple, check out
'Protocols.Df.fanout'.
-}
repeatWithIndexC ::
  forall n a b.
  (KnownNat n) =>
  (Index n -> Circuit a b) ->
  Circuit (Vec n a) (Vec n b)
repeatWithIndexC f =
  Circuit (unzip . zipWith g indicesI . uncurry zip)
 where
  g i = case f i of Circuit f' -> f'

-- Was not allowed to make these due to circular dependencies (Protocols.Df.Extra in bittide-extra, but bittide-extra depends on clash-protocols-memmap)

-- {- | Creates a `Df` wrapper around a block RAM primitive that supports byte enables for
-- its write channel. Writes are always acked immediately, reads receive backpressure
-- based on the outgoing `Df` channel.
-- -}
-- fromBlockramWithMask ::
--   (HiddenClockResetEnable dom, Num addr, NFDataX addr, KnownNat words) =>
--   ( Enable dom ->
--     Signal dom addr ->
--     Signal dom (Maybe (addr, BitVector (words * 8))) ->
--     Signal dom (BitVector words) ->
--     Signal dom (BitVector (words * 8))
--   ) ->
--   Circuit
--     ( BiDf dom addr (BitVector (words * 8))
--     , Df dom (addr, BitVector words, BitVector (words * 8))
--     )
--     ()
-- fromBlockramWithMask primitive = circuit $ \(bidf, writeData) -> do
--   readAddress <- toDfs -< (bidf, readData)
--   readData <- Df.fromBlockramWithMask primitive -< (readAddress, writeData)
--   idC -< ()

-- {- | Creates a `Df` wrapper around a block RAM primitive. Writes are always acked
-- immediately, reads receive backpressure based on the outgoing `Df` channel.
-- -}
-- fromBlockram ::
--   (HiddenClockResetEnable dom, Num addr, NFDataX addr, NFDataX a) =>
--   ( Enable dom ->
--     Signal dom addr ->
--     Signal dom (Maybe (addr, BitVector (words * 8))) ->
--     Signal dom (BitVector (words * 8))
--   ) ->
--   Circuit
--     ( BiDf dom addr (BitVector (words * 8))
--     , Df dom (addr, BitVector (words * 8))
--     )
--     ()
-- fromBlockram primitive = circuit $ \(bidf, writeData) -> do
--   readAddress <- toDfs -< (bidf, readData)
--   readData <- Df.fromBlockram primitive -< (readAddress, writeData)
--   idC -< ()
