-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- |
Contains the `ReqResp` protocol, the simplest possible protocol for request-response communication
together with utilities for working with it. `ReqResp` is suitable for simple request-response
interactions where pipelining is not required. If you need pipelining, you can use the `BiDf`
protocol instead.
-}
module Protocols.ReqResp (
  -- * Types
  ReqResp,

  -- * Converters
  fromDfs,
  toDfs,
  fromBiDf,
  toBiDf,
  requests,
  dropResponse,

  -- * Blockram interfaces
  fromBlockRam,
  fromBlockRamWithMask,

  -- * Other utilities
  partitionEithers,
  forceResetSanity,
) where

import Clash.Prelude

import Data.Bifunctor (Bifunctor (..))
import Data.Maybe
import Protocols
import Protocols.BiDf (BiDf)
import Protocols.Idle

import qualified Clash.Prelude as C
import qualified Protocols.BiDf as BiDf

{- |
Simplest possible protocol for request-response communication.

The forward channel channel has type @Signal dom (Maybe req)@ and is used to send requests and
the backward channel has type @Signal dom (Maybe resp)@ and is used to send responses. This protocol
can not be pipelined because the request is acknowledged by the response. If you wish to utilize
pipelining, you can use the `BiDf` protocol instead.

The protocol must obey the following rules:
* When the forward channel is @Just a@, it must not change until the transaction is completed.
* The forward channel can not depend on the backward channel.
* When the forward channel is @Nothing@, the backward channel must not be observed.
-}
data ReqResp (dom :: C.Domain) (req :: Type) (resp :: Type)

instance Protocol (ReqResp dom req resp) where
  -- \| Request channel for ReqResp protocol
  type Fwd (ReqResp dom req resp) = C.Signal dom (Maybe req)

  -- \| Response channel for ReqResp protocol
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

-- | Partition a `ReqResp` with an `Either` request type into two `ReqResp`s, one for each side of the `Either`.
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

{- | Given a 'blockRam' primitive, create a circuit that offers a 'ReqResp' interface to access
the primitive using 'ReqResp' for the read channel and 'Df' for the write channel.

If you use 'ReqResp' for the write channel, you can use 'requests' to convert it to a 'Df' stream
to be used with this circuit.
-}
fromBlockRamWithMask ::
  (C.KnownDomain dom, C.HiddenClock dom, C.HiddenReset dom, Num addr, C.KnownNat words) =>
  ( C.Signal dom addr ->
    C.Signal dom (Maybe (addr, C.BitVector (words C.* 8))) ->
    C.Signal dom (C.BitVector words) ->
    C.Signal dom (C.BitVector (words C.* 8))
  ) ->
  Circuit
    ( ReqResp dom addr (C.BitVector (words C.* 8))
    , Df dom (addr, C.BitVector words, C.BitVector (words C.* 8))
    )
    ()
fromBlockRamWithMask primitive = Circuit go
 where
  writeBwd = pure $ Ack True
  go ((readFwd, writeFwd), _) = ((readBwd, writeBwd), ())
   where
    -- Separate the write data and byte enables
    writeData = fmap (>>= \(addr, _mask, dat) -> Just (addr, dat)) writeFwd
    byteEnables = fmap (\case Just (_, mask, _) -> mask; Nothing -> 0) writeFwd

    readData = primitive (fromMaybe 0 <$> readFwd) writeData byteEnables

    -- Reading takes 1 cycle so we run at half speed
    readValid = C.withEnable C.enableGen C.register False (fmap isJust readFwd C..&&. fmap not readValid)
    readBwd = liftA2 (\v d -> if v then Just d else Nothing) readValid readData

{- | Given a 'blockRam' primitive, create a circuit that offers a 'ReqResp' interface to access
the primitive using 'ReqResp' for the read channel and 'Df' for the write channel.
This is a simpler version of 'fromBlockRamWithMask' that doesn't support byte enables.
-}
fromBlockRam ::
  (C.KnownDomain dom, C.HiddenClock dom, C.HiddenReset dom, Num addr) =>
  (C.Signal dom addr -> C.Signal dom (Maybe (addr, a)) -> C.Signal dom a) ->
  Circuit (ReqResp dom addr a, Df dom (addr, a)) ()
fromBlockRam primitive = Circuit go
 where
  writeBwd = pure $ Ack True
  go ((readFwd, writeFwd), _) = ((readBwd, writeBwd), ())
   where
    readData = primitive (fromMaybe 0 <$> readFwd) writeFwd

    -- Reading takes 1 cycle so we run at half speed
    readValid = C.withEnable C.enableGen C.register False (fmap isJust readFwd C..&&. fmap not readValid)
    readBwd = liftA2 (\v d -> if v then Just d else Nothing) readValid readData

{- | Forces a constant response on the backward channel. Useful for when you don't care
about the response data.
-}
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
toDfs = ckt
 where
  ckt = Circuit (first C.unbundle . C.unbundle . C.mealy go Nothing . C.bundle . first C.bundle)
  go Nothing _ = (Just False, ((Nothing, Ack False), Nothing))
  go (Just accepted0) ~(~(reqLeft, resp), ~(Ack reqRightAck)) = (Just accepted1, ((resp, respAck), reqRight))
   where
    respAck = Ack True

    reqRight
      | accepted0 = Nothing
      | otherwise = reqLeft

    accepted1
      | isNothing reqLeft = False -- No request to accept
      | isJust resp = False -- Receiving a response clears the state
      | isJust reqRight = reqRightAck -- A request for which we have not received a response yet
      | otherwise = accepted0

-- | Convert two `Df` streams for requests and responses into a `ReqResp` protocol.
fromDfs ::
  forall dom req resp.
  (C.HiddenClockResetEnable dom, C.NFDataX resp) =>
  Circuit (Df dom req) (ReqResp dom req resp, Df dom resp)
fromDfs = Circuit (second C.unbundle . C.unbundle . C.mealy go Nothing . C.bundle . second C.bundle)
 where
  go Nothing ~(req, ~(resp, Ack ack)) = (nextState, (Ack (isJust resp), (req, resp)))
   where
    nextState
      | isJust resp && not ack = resp
      | otherwise = Nothing
  go stored (_, (_, Ack ack)) = (nextState, (Ack False, (Nothing, stored)))
   where
    nextState
      | ack = Nothing
      | otherwise = stored

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
  (C.HiddenClockResetEnable dom, C.NFDataX resp) =>
  Circuit (BiDf dom req resp) (ReqResp dom req resp)
fromBiDf = circuit $ \biDf -> do
  request <- BiDf.toDfs -< (biDf, response)
  (reqresp, response) <- fromDfs -< request
  idC -< reqresp

-- | Convert a 'ReqResp' protocol where the response type is '()' to a 'Df' stream of requests.
requests ::
  forall dom req.
  (C.KnownDomain dom) =>
  Circuit (ReqResp dom req ()) (Df dom req)
requests = Circuit (C.unbundle . fmap go . C.bundle)
 where
  go ~(request, Ack ack) = (if ack then Just () else Nothing, request)
