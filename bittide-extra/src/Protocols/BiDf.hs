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
  fromBiDf,
  toBiDf,

  -- * Trivial combinators
  void,
  loopback,

  -- * Mapping
  mapC,
  dimap,

  -- * Fan-in
  fanin,

  -- * Complex combinators
  prefetch,

  -- * Memories
  fromBlockram,
  fromBlockramWithMask,

  -- * Debugging
  trace,
  -- , traceSignal
) where

import Prelude ()

import Clash.Prelude hiding (traceSignal)
import Data.Bifunctor
import Data.Maybe (isJust, isNothing)
import Data.Typeable (Typeable)
import Protocols

import qualified Protocols.Df as Df
import qualified Protocols.Df.Extra as Df

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

data PrefetchState req = Empty | OfferingRequest req | WaitingForResponse req
  deriving (Generic, NFDataX, BitPack, Show, ShowX, Eq)

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
  ( HiddenClockResetEnable dom
  , Eq req
  , Enum req
  , NFDataX req
  , NFDataX resp
  ) =>
  Circuit (BiDf dom req resp) (BiDf dom req resp)
prefetch =
  Circuit (bimap unbundle unbundle . unbundle . mealy go Empty . bundle . bimap bundle bundle)
 where
  go storedReq0 ~(~(reqLeft, Ack respLeftAck0), ~(Ack reqRightAck, respRight)) =
    (storedReq1, ((Ack reqLeftAck, respLeft), (reqRight, Ack respRightAck)))
   where
    -- Check if we have a cache hit: user request matches prefetched request
    emptyReg = storedReq0 == Empty
    (hit, miss) = case (storedReq0, reqLeft) of
      (WaitingForResponse prefReq, Just req) | isJust respRight -> (req == prefReq, req /= prefReq)
      _ -> (False, False)

    -- hit = reqLeft == storedReq0 && isJust respRight
    -- miss = isJust reqLeft && not hit && isJust respRight
    requestAccepted = isJust reqRight && reqRightAck
    responseAccepted = isJust respLeft && respLeftAck0

    respRightAck = if miss then True else responseAccepted -- Discard prefetched response on miss
    reqLeftAck = isJust reqLeft && responseAccepted

    -- Determine what request to issue
    reqRight = case storedReq0 of
      Empty -> reqLeft -- No prefetched request, just forward user request
      (OfferingRequest prefReq) -> Just prefReq -- Keep offering prefetched request until it's accepted
      (WaitingForResponse prefReq) | responseAccepted -> Just $ succ prefReq
      -- (WaitingForResponse _) | miss             -> reqLeft -- Cache miss, drop prefetched request and forward user request
      (WaitingForResponse _) -> Nothing

    -- Output response handling
    respLeft
      | hit = respRight -- Cache hit: forward response
      | otherwise = Nothing

    -- Update prefetched request
    storedReq1
      | requestAccepted = WaitingForResponse (fromJustX reqRight) -- Some request has been accepted
      | responseAccepted = OfferingRequest (fromJustX reqRight) -- Cache hit, but request not accepted. Keep offering the same request until it is accepted.
      | emptyReg = maybe Empty OfferingRequest reqLeft -- No pending request, wait for user request
      | miss = Empty -- Cache miss, drop stored request
      | otherwise = storedReq0 -- Keep current request

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

-- | Map requests and responses of a 'BiDf' using separate `Df` circuits.
mapC ::
  Circuit (Df dom req) (Df dom req') ->
  Circuit (Df dom resp) (Df dom resp') ->
  Circuit (BiDf dom req resp') (BiDf dom req' resp)
mapC mapReq mapResp = circuit $ \bidf -> do
  req <- toDfs -< (bidf, resp')
  req' <- mapReq -< req
  resp' <- mapResp -< resp
  (bidf', resp) <- fromDfs -< req'
  idC -< bidf'

-- | Map both requests and responses.
dimap ::
  (req -> req') ->
  (resp -> resp') ->
  Circuit (BiDf dom req resp') (BiDf dom req' resp)
dimap f g = mapC (Df.map f) (Df.map g)

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

{- | Creates a `Df` wrapper around a block RAM primitive that supports byte enables for
its write channel. Writes are always acked immediately, reads receive backpressure
based on the outgoing `Df` channel.
-}
fromBlockramWithMask ::
  (HiddenClockResetEnable dom, Num addr, NFDataX addr, KnownNat words) =>
  ( Enable dom ->
    Signal dom addr ->
    Signal dom (Maybe (addr, BitVector (words * 8))) ->
    Signal dom (BitVector words) ->
    Signal dom (BitVector (words * 8))
  ) ->
  Circuit
    ( BiDf dom addr (BitVector (words * 8))
    , Df dom (addr, BitVector words, BitVector (words * 8))
    )
    ()
fromBlockramWithMask primitive = circuit $ \(bidf, writeData) -> do
  readAddress <- toDfs -< (bidf, readData)
  readData <- Df.fromBlockramWithMask primitive -< (readAddress, writeData)
  idC -< ()

{- | Creates a `Df` wrapper around a block RAM primitive. Writes are always acked
immediately, reads receive backpressure based on the outgoing `Df` channel.
-}
fromBlockram ::
  forall dom addr words.
  (HiddenClockResetEnable dom, KnownNat words, Num addr, NFDataX addr) =>
  ( Enable dom ->
    Signal dom addr ->
    Signal dom (Maybe (addr, BitVector (words * 8))) ->
    Signal dom (BitVector (words * 8))
  ) ->
  Circuit
    ( BiDf dom addr (BitVector (words * 8))
    , Df dom (addr, BitVector (words * 8))
    )
    ()
fromBlockram primitive = circuit $ \(bidf, writeData) -> do
  readAddress <- toDfs -< (bidf, readData)
  readData <- Df.fromBlockram primitive -< (readAddress, writeData)
  idC -< ()

{- | `BiDf` version of `traceShowId`, introduces no state or logic of any form. Only prints when
there is data available on the input side. Prints available data, clock cycle count in the
relevant domain, and the corresponding Ack.
-}
trace ::
  (KnownDomain dom, ShowX req, NFDataX req, ShowX resp, NFDataX resp) =>
  String ->
  Circuit (BiDf dom req resp) (BiDf dom req resp)
trace msg = mapC (Df.trace (msg <> "_request")) (Df.trace (msg <> "_response"))

{- | `BiDf` version of `Clash.Debug.traceSignal` based on `Df.traceSignal`.
Signal names:
- left request forward: name_request_fwd
- left request backward: name_request_bwd
- right response forward: name_response_fwd
- right response backward: name_response_bwd
-}
traceSignal ::
  ( KnownDomain dom
  , ShowX req
  , NFDataX req
  , BitPack req
  , Typeable req
  , ShowX resp
  , NFDataX resp
  , BitPack resp
  , Typeable resp
  ) =>
  String ->
  Circuit (BiDf dom req resp) (BiDf dom req resp)
traceSignal name = mapC (Df.traceSignal (name <> "_request")) (Df.traceSignal (name <> "_response"))
