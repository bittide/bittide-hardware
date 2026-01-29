-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Protocols.Df.Extra where

import Clash.Prelude
import Data.Maybe
import Data.String.Interpolate (i)
import Protocols
import Protocols.Df (forceResetSanity)

import qualified Clash.Explicit.Prelude as E
import qualified Clash.Explicit.Signal.Delayed as ED
import qualified Clash.Explicit.Signal.Delayed.Extra as ED
import qualified Clash.Signal.Delayed as D
import qualified Debug.Trace as Debug
import qualified Protocols.Df as Df

andAck :: forall dom a. Signal dom Bool -> Circuit (Df dom a) (Df dom a)
andAck extraAcks = Circuit go0
 where
  go0 :: (Signal dom (Maybe a), Signal dom Ack) -> (Signal dom Ack, Signal dom (Maybe a))
  go0 (as, acks) = (go1 <$> acks <*> extraAcks, as)

  go1 :: Ack -> Bool -> Ack
  go1 (Ack ack) extraAck = Ack (ack && extraAck)

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
    ( Df dom addr
    , Df dom (addr, BitVector words, BitVector (words * 8))
    )
    (Df dom (BitVector (words * 8)))
fromBlockramWithMask primitive = circuit $ \(r, w) -> do
  Fwd (D.fromSignal -> writeOp) <- Df.toMaybe <| forceResetSanity -< w
  let
    write = fmap (\(addr, _, dat) -> (addr, dat)) <$> writeOp
    mask = maybe 0 (\(_, mask', _) -> mask') <$> writeOp
    primitiveD ena readD = ED.fromBlockramWithMask (primitive ena) readD write mask
  fromDSignal hasClock hasReset hasEnable primitiveD <| forceResetSanity -< r

{- | Creates a `Df` wrapper around a block RAM primitive. Writes are always acked
immediately, reads receive backpressure based on the outgoing `Df` channel.
-}
fromBlockram ::
  (HiddenClockResetEnable dom, Num addr, NFDataX addr, NFDataX a) =>
  (Enable dom -> Signal dom addr -> Signal dom (Maybe (addr, a)) -> Signal dom a) ->
  Circuit (Df dom addr, Df dom (addr, a)) (Df dom a)
fromBlockram primitive = circuit $ \(r, w) -> do
  Fwd (D.fromSignal -> write) <- Df.toMaybe <| forceResetSanity -< w
  let primitiveD ena readD = ED.fromBlockram (primitive ena) readD write
  fromDSignal hasClock hasReset hasEnable primitiveD <| forceResetSanity -< r

-- | Converts a delay annotated circuit with enable port into a `Df` circuit.
fromDSignal ::
  forall dom a b n.
  ( KnownDomain dom
  , NFDataX a
  , NFDataX b
  , KnownNat n
  ) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  (Enable dom -> D.DSignal dom 0 a -> D.DSignal dom n b) ->
  Circuit (Df dom a) (Df dom b)
fromDSignal clk rst ena0 f = withReset rst Df.forceResetSanity |> Circuit go
 where
  go (dataLeft, ackRight) = (fmap Ack ackLeft, D.toSignal dataRight)
   where
    ackLeft = fmap not (D.toSignal dataRightValid) .||. fmap (\(Ack ack) -> ack) ackRight
    dataLeftValid = fmap isJust dataLeft
    dataRightValid = ED.delayI False ena1 clk $ D.fromSignal dataLeftValid
    dataRight = liftA2 (\v d -> if v then Just d else Nothing) dataRightValid data_
    ena1 = E.andEnable ena0 ackLeft
    data_ = f ena1 (D.fromSignal (fromJustX <$> dataLeft))

-- | Generates an infinite stream of values by repeatedly applying a function.
iterate ::
  forall dom a.
  (HiddenClockResetEnable dom, NFDataX a) =>
  (a -> a) ->
  a ->
  Circuit () (Df dom a)
iterate f s0 = Circuit (((),) . mealy go s0 . snd)
 where
  go now (Ack respAck) = (next, Just now)
   where
    stalled = not respAck
    next
      | stalled = now
      | otherwise = f now

trace ::
  (KnownDomain dom, ShowX a, NFDataX a) =>
  String ->
  Circuit (Df dom a) (Df dom a)
trace msg =
  Circuit
    (unbundle . withClockResetEnable clockGen resetGen enableGen mealy go (0 :: Int) . bundle)
 where
  go cnt (m2s, s2m) = (cnt + 1, (s2m, fmap f m2s))
   where
    f m = Debug.trace [i| Df.Trace #{msg} | #{cnt}: #{showX m}, #{showX s2m}|] m
