-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Protocols.Df.Extra where

import Clash.Prelude hiding (traceSignal)
import Data.Bifunctor (Bifunctor (..))
import Data.Maybe
import Data.String.Interpolate (i)
import Data.Typeable (Typeable)
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

-- | Indicates whether the downstream is ready to accept data
type Ready = Bool

data SkidState a
  = InReset
  | Empty
  | Full a
  deriving (Generic, NFDataX)

{- | A skid buffer that can store a single element and will acknowledge data if it can
store it. Also indicates when it is ready to accept data. In practice, the ready signal is
the exact same as the ack signal, but 'Df' disallows interpreting the ack signal as a
this-component-is-ready-to-accept-data signal.

This skid buffer is implemented in terms of 'moore', so it cuts up combinational paths.
-}
skid ::
  forall dom a.
  (NFDataX a, HiddenClockResetEnable dom) =>
  Circuit (Df dom a) (Df dom a, CSignal dom Ready)
skid = Circuit go
 where
  go (dataIn, (ackIn, _)) = (ackOut, (dataOut, readyOut))
   where
    (ackOut, dataOut, readyOut) = mooreB goState goOutput InReset (dataIn, ackIn)

  goState :: SkidState a -> (Maybe a, Ack) -> SkidState a
  goState InReset _ = Empty
  goState Empty (Nothing, _) = Empty
  goState Empty (Just dat, _) = Full dat
  goState s@(Full _) (_, ~(Ack ack)) = if ack then Empty else s

  goOutput :: SkidState a -> (Ack, Maybe a, Bool)
  goOutput s = let ack = isEmpty s in (Ack ack, toMaybe s, ack)

  isEmpty :: SkidState a -> Bool
  isEmpty Empty = True
  isEmpty _ = False

  toMaybe :: SkidState a -> Maybe a
  toMaybe (Full a) = Just a
  toMaybe _ = Nothing

ackWhen :: Signal dom Bool -> Circuit (Df dom a) ()
ackWhen canDrop = Circuit $ \_ -> (Ack <$> canDrop, ())

{- | Creates a `Df` wrapper around a block RAM primitive that supports byte enables for
its write channel. Writes are always acked immediately, reads receive backpressure
based on the outgoing `Df` channel.
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
    ( Df dom addr
    , Df dom (addr, BitVector words, BitVector (words * 8))
    )
    (Df dom (BitVector (words * 8)))
fromBlockRamWithMask primitive = circuit $ \(r, w) -> do
  Fwd (D.fromSignal -> writeOp) <- Df.toMaybe <| forceResetSanity -< w
  let
    write = fmap (\(addr, _, dat) -> (addr, dat)) <$> writeOp
    mask = maybe 0 (\(_, mask', _) -> mask') <$> writeOp
    primitiveD ena readD = ED.fromBlockRamWithMask (primitive ena) readD write mask
  fromDSignal hasClock hasReset primitiveD <| forceResetSanity -< r

fromBlockRam ::
  (KnownDomain dom, HiddenClock dom, HiddenReset dom, Num addr, NFDataX addr, NFDataX a) =>
  (Enable dom -> Signal dom addr -> Signal dom (Maybe (addr, a)) -> Signal dom a) ->
  Circuit (Df dom addr, Df dom (addr, a)) (Df dom a)
fromBlockRam primitive = circuit $ \(r, w) -> do
  Fwd (D.fromSignal -> write) <- Df.toMaybe <| forceResetSanity -< w
  let primitiveD ena readD = ED.fromBlockRam (primitive ena) readD write
  fromDSignal hasClock hasReset primitiveD <| forceResetSanity -< r

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
  (Enable dom -> D.DSignal dom 0 a -> D.DSignal dom n b) ->
  Circuit (Df dom a) (Df dom b)
fromDSignal clk rst f = withReset rst Df.forceResetSanity |> Circuit go
 where
  go (dataLeft, ackRight) = (fmap Ack ackLeft, D.toSignal dataRight)
   where
    ackLeft = fmap not (D.toSignal dataRightValid) .||. fmap (\(Ack ack) -> ack) ackRight
    dataLeftValid = fmap isJust dataLeft
    dataRightValid = ED.delayI False ena clk $ D.fromSignal dataLeftValid
    dataRight = liftA2 (\v d -> if v then Just d else Nothing) dataRightValid data_
    ena = E.toEnable ackLeft
    data_ = f ena (D.fromSignal (fromJustX <$> dataLeft))

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

data BypassState a maxDelay
  = BypassState
  { inReset :: Bool
  , stored :: Maybe a
  , count :: Index (maxDelay + 1)
  }
  deriving (Generic, NFDataX)

{- | Fifos inherently have latency, this circuit allows you to bypass the fifo when it is empty
to allow for 0 latency communication when possible, while still adhering to the Df protocol.
-}
bypassFifo ::
  forall dom maxDelay a.
  (HiddenClockResetEnable dom, NFDataX a, 1 <= maxDelay) =>
  SNat maxDelay ->
  Circuit (Df dom a) (Df dom a) ->
  Circuit (Df dom a) (Df dom a)
bypassFifo SNat fifoCircuit = circuit $ \inp -> do
  fifoOut <- fifoCircuit -< fifoIn
  (out, fifoIn) <- bypassCkt -< (inp, fifoOut)
  idC -< out
 where
  bypassCkt =
    Circuit
      ( bimap unbundle unbundle
          . unbundle
          . mealy go initState
          . bundle
          . bimap bundle bundle
      )
  initState :: BypassState a maxDelay
  initState = BypassState{inReset = True, stored = Nothing, count = 0}

  -- Reset state
  go state ~(~(inp, fifoOut), ~(Ack outAck, Ack fifoInAck))
    | state.inReset = (initState{inReset = False}, ((Ack False, Ack False), (Nothing, Nothing)))
    | isNothing state.stored && state.count == 0 =
        let
          inpAck = Ack True
          fifoOutAck = Ack False
          out = inp
          fifoIn = Nothing
          -- If we receive backpressure while trying to bypass, buffer the input
          -- to adhere to the Df protocol.
          nextReg
            | isJust inp && not outAck = inp
            | otherwise = Nothing

          nextCount = 0
          nextState = BypassState{inReset = False, stored = nextReg, count = nextCount}
         in
          (nextState, ((inpAck, fifoOutAck), (out, fifoIn)))
    | isJust state.stored =
        let
          fifoIn = inp
          inpAck = Ack fifoInAck
          out = state.stored
          fifoOutAck = Ack False

          -- We receive backpressure on a buffered valued, if we receive a new value we have to
          -- put it in the fifo and thus increase the count
          nextCount
            | isJust inp = maxBound
            | otherwise = state.count

          nextReg = if outAck then Nothing else state.stored
          nextState = BypassState{inReset = False, stored = nextReg, count = nextCount}
         in
          (nextState, ((inpAck, fifoOutAck), (out, fifoIn)))
    -- Fifo is not empty, count is nonzero.
    | otherwise =
        let
          inpAck = Ack fifoInAck
          fifoOutAck = Ack outAck
          out = fifoOut
          fifoIn = inp

          -- When the fifo produces a sample, reset the count to maxBound
          nextCount
            | isJust fifoOut = maxBound
            | otherwise = satPred SatZero state.count

          nextReg = Nothing
          nextState = BypassState{inReset = False, stored = nextReg, count = nextCount}
         in
          (nextState, ((inpAck, fifoOutAck), (out, fifoIn)))

{- | `Df` version of `traceShowId`, introduces no state or logic of any form. Only prints when
there is data available on the input side. Prints available data, clock cycle count in the
relevant domain, and the corresponding Ack.
-}
trace ::
  (KnownDomain dom, ShowX a, NFDataX a) =>
  String ->
  Circuit (Df dom a) (Df dom a)
trace msg =
  Circuit
    (unbundle . withClockResetEnable clockGen resetGen enableGen mealy go (0 :: Int) . bundle)
 where
  go cnt ~(m2s, s2m) = (cnt + 1, (s2m', m2s))
   where
    s2m' = Debug.trace [i| Df.Trace #{msg} | #{cnt}: #{showX m2s}, #{showX s2m}|] s2m

-- | `Df` version of `Clash.Debug.traceSignal`. names forward signal (name_fwd) and backward signal (name_bwd)
traceSignal ::
  (KnownDomain dom, ShowX a, NFDataX a, BitPack a, Typeable a) =>
  String ->
  Circuit (Df dom a) (Df dom a)
traceSignal name = Circuit go
 where
  go ~(fwd, bwd) = (E.traceSignal (name <> "_bwd") bwd, E.traceSignal (name <> "_fwd") fwd)
