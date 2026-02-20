-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

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
  map,
  bimap,

  -- * Fan-in
  fanin,

  -- * Memories
  fromBlockRam,
  fromBlockRamWithMask,

  -- * Debugging
  trace,
  traceSignal,
) where

import Clash.Prelude hiding (map, traceSignal)
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

-- | Ignore all requests, never providing responses.
void :: (KnownDomain dom, HiddenReset dom) => Circuit (BiDf dom req resp') ()
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
map ::
  Circuit (Df dom req) (Df dom req') ->
  Circuit (Df dom resp) (Df dom resp') ->
  Circuit (BiDf dom req resp') (BiDf dom req' resp)
map mapReq mapResp = circuit $ \bidf -> do
  req <- toDfs -< (bidf, resp')
  req' <- mapReq -< req
  resp' <- mapResp -< resp
  (bidf', resp) <- fromDfs -< req'
  idC -< bidf'

-- | Map both requests and responses.
bimap ::
  (req -> req') ->
  (resp -> resp') ->
  Circuit (BiDf dom req resp') (BiDf dom req' resp)
bimap f g = map (Df.map f) (Df.map g)

{- | Merge a number of 'BiDf's, preferring requests from the last channel.
TODO: Check why this does not work if we insert delays on the individual `Df` channels of the
right hand side `BiDf`s. See `Tests.Protocols.BiDf.prop_fanin`.
-}
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
  let
    (reqDatas, respAcks) = unzip upFwds

    ((reqAcks, respAck), (respDatas, reqData)) =
      toSignals fanin' ((reqDatas, respData), (respAcks, reqAck))
   in
    (zip reqAcks respDatas, (reqData, respAck))
 where
  fanin' ::
    Circuit
      (Vec n (Df dom req), Df dom resp)
      (Vec n (Df dom resp), Df dom req)
  fanin' = circuit $ \(reqs, resp0) -> do
    [fwd0, fwd1] <-
      Df.fanout
        <| Df.roundrobinCollect @n Df.Parallel
        <| repeatWithIndexC (\i -> Df.map (\x -> (i, x)))
        -< reqs

    (activeN, Fwd rdy) <- Df.skid <| Df.map fst -< fwd1
    req1 <- Df.stallNext rdy -< req0
    resps <- Df.route <| Df.zip -< (activeN, resp0)
    req0 <- Df.map snd -< fwd0
    idC -< (resps, req1)

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

This component assumes that the blockram primitive produces a result one cycle after the read
address is provided and that it can be stalled by deasserting the enable signal.

TODO: Use `DSignal` for primitive to enforce the timing assumption on a type level.
-}
fromBlockRamWithMask ::
  (KnownDomain dom, HiddenClock dom, HiddenReset dom, Num addr, NFDataX addr, KnownNat words) =>
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
fromBlockRamWithMask primitive = circuit $ \(bidf, writeData) -> do
  readAddress <- toDfs -< (bidf, readData)
  readData <- Df.fromBlockRamWithMask primitive -< (readAddress, writeData)
  idC -< ()

{- | Creates a `Df` wrapper around a block RAM primitive. Writes are always acked
immediately, reads receive backpressure based on the outgoing `Df` channel.

This component assumes that the blockram primitive produces a result one cycle after the read
address is provided and that it can be stalled by deasserting the enable signal.

TODO: Use `DSignal` for primitive to enforce the timing assumption on a type level.
-}
fromBlockRam ::
  forall dom addr words.
  (KnownDomain dom, HiddenClock dom, HiddenReset dom, KnownNat words, Num addr, NFDataX addr) =>
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
fromBlockRam primitive = circuit $ \(bidf, writeData) -> do
  readAddress <- toDfs -< (bidf, readData)
  readData <- Df.fromBlockRam primitive -< (readAddress, writeData)
  idC -< ()

{- | `BiDf` version of `traceShowId`, introduces no state or logic of any form. Only prints when
there is data available on the input side. Prints available data, clock cycle count in the
relevant domain, and the corresponding Ack.
-}
trace ::
  (KnownDomain dom, ShowX req, NFDataX req, ShowX resp, NFDataX resp) =>
  String ->
  Circuit (BiDf dom req resp) (BiDf dom req resp)
trace msg = map (Df.trace (msg <> "_request")) (Df.trace (msg <> "_response"))

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
traceSignal name = map (Df.traceSignal (name <> "_request")) (Df.traceSignal (name <> "_response"))
