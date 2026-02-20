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

{- | Force a @Nothing@ on the backward channel and @Nothing@ on the forward
channel if reset is asserted.
-}
forceResetSanity ::
  forall dom req resp.
  (C.HiddenReset dom) =>
  Circuit (ReqResp dom req resp) (ReqResp dom req resp)
forceResetSanity = forceResetSanityGeneric
