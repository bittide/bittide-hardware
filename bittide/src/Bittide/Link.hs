-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bittide.Link where

import Clash.Prelude

import Bittide.Extra.Wishbone
import Bittide.SharedTypes
import Bittide.DoubleBufferedRam
import Data.Constraint
import Data.Constraint.Nat.Extra
import Data.Maybe

-- Internal states of the txUnit.
data TransmissionState preambleWidth seqCountWidth frameWidth
  = LinkThrough
  -- ^ The txUnit is transparent, the incoming frame is directly routed to the output.
  | TransmitPreamble (Index (Regs (BitVector preambleWidth) frameWidth))
  -- ^ The txUnit is transmitting the preamble, the index keeps track of which frame of
  -- the preamble is being transmitted.
  | TransmitSeqCounter (Index (Regs (BitVector seqCountWidth) frameWidth))
  -- ^ The txUnit is transmitting the stored sequence counter, the index keeps track
  -- of which frame of the sequence counter is being transmitted.
   deriving (Generic, NFDataX)

{-# NOINLINE txUnit #-}
-- | Transmitter for the Bittide Link, it either transmits the incoming gather frame or
-- transmits the preamble followed by the sequence counter.
txUnit ::
  forall core nBytes aw preambleWidth frameWidth seqCountWidth .
  ( HiddenClockResetEnable core
  , KnownNat preambleWidth
  , KnownNat seqCountWidth
  , KnownNat frameWidth
  , KnownNat nBytes
  , 1 <= nBytes
  , KnownNat aw
  , 2 <= aw
  , 1 <= frameWidth) =>
  -- |  Hardcoded preamble.
  BitVector preambleWidth ->
  -- | Local sequence counter.
  Signal core (Unsigned seqCountWidth) ->
  -- | Frame from 'gatherUnitWb'
  Signal core (DataLink frameWidth) ->
  -- | Control register Wishbone bus (Master -> slave).
  Signal core (WishboneM2S nBytes aw) ->
  -- |
  -- 1. Control register Wishbone bus (Slave -> master).
  -- 2. Outgoing frame
  ( Signal core (WishboneS2M nBytes)
  , Signal core (DataLink frameWidth))
txUnit (getRegs -> RegisterBank preamble) sq frameIn wbIn = (wbOut, frameOut)
 where
  (stateMachineOn, wbOut)
    | Dict <- timesDivRU @(nBytes * 8) @1
    = registerWb WishbonePriority False wbIn (pure Nothing)

  frameOut = withReset regReset $ mealy stateMachine (scErr, LinkThrough) mealyIn
  mealyIn = bundle (frameIn, sq)
  regReset = forceReset $ not <$> stateMachineOn

  stateMachine ::
    (Unsigned seqCountWidth, TransmissionState preambleWidth seqCountWidth frameWidth) ->
    (DataLink frameWidth, Unsigned seqCountWidth) ->
    ( (Unsigned seqCountWidth
    , TransmissionState preambleWidth seqCountWidth frameWidth)
    , DataLink frameWidth)
  stateMachine (scStored@(getRegs -> RegisterBank sqVec), state) (fIn, scIn) =
    ((nextSc, nextState state), out)
   where
    (nextSc, out) = case state of
      LinkThrough          -> (scStored, fIn)
      TransmitSeqCounter n -> (scStored, Just $ sqVec !! n)
      TransmitPreamble n
        | n == maxBound -> (scIn,     Just $ preamble !! n)
        | otherwise     -> (scStored, Just $ preamble !! n)
  scErr = deepErrorX "txUnit: Stored sequence counter invalid"

  -- Once turned on, the txUnit continues to transmit the preamble followed by the sequence
  -- counter.
  nextState ::
    (KnownNat pw, KnownNat scw, KnownNat fw, 1 <= fw) =>
    TransmissionState pw scw fw ->
    TransmissionState pw scw fw
  nextState = \case
      LinkThrough       -> TransmitPreamble 0
      TransmitPreamble n
        | n == maxBound -> TransmitSeqCounter 0
        | otherwise     -> TransmitPreamble (succ n)
      TransmitSeqCounter n
        | n == maxBound -> TransmitPreamble 0
        | otherwise     -> TransmitSeqCounter (succ n)

-- | States for the rxUnit.
data ReceiverState
  = Idle
  -- ^ Receiver is in idle state.
  | WaitingForPreamble
  -- ^ Receiver is waiting for the preamble to be detected.
  | CaptureSequenceCounter
  -- ^ Receiver is capturing the sequence counter.
  deriving (Generic, ShowX, BitPack)

-- | The width of the internal shift register used for capturing the preamble and two
-- sequence counters.
type ShiftRegWidth paw scw = Max paw (scw + scw)

{-# NOINLINE rxUnit #-}
-- | Receives a Bittide link and can be set to detect the given preamble and capture the
-- following sequence counter.
rxUnit ::
  forall core nBytes aw paw fw scw .
  ( HiddenClockResetEnable core
  , KnownNat nBytes, 1 <= nBytes
  , KnownNat aw, 2 <= aw
  , KnownNat paw, 1 <= paw
  , KnownNat fw, 1 <= fw
  , KnownNat scw, 1 <= scw) =>
  -- | Preamble.
  BitVector paw ->
  -- | Local sequence counter.
  Signal core (Unsigned scw) ->
  -- | Incoming bittide link.
  Signal core (DataLink fw) ->
  -- | Control register Wishbone bus (Master -> slave).
  Signal core (WishboneM2S nBytes aw) ->
  -- | Control register Wishbone bus (Slave -> master).
  Signal core (WishboneS2M nBytes)
rxUnit preamble localCounter linkIn wbIn = wbOut
 where
  (regOut, wbOut) = registerWbE WishbonePriority regInit wbIn regIn byteEnables
  regInit = (0,resize $ pack Idle)
  (regIn, byteEnables) = unbundle . mealy go 0 $ bundle (regOut, linkIn, localCounter)

  go ::
    Index (DivRU scw fw) ->
    ((BitVector (ShiftRegWidth paw scw), Bytes nBytes), DataLink fw, Unsigned scw) ->
    ( Index (DivRU scw fw)
    , (Maybe (BitVector (ShiftRegWidth paw scw), Bytes nBytes)
    , ByteEnable (BitVector (ShiftRegWidth paw scw + nBytes * 8))))
  go _ ((shiftOld, unpack . resize -> WaitingForPreamble), link, _) =
    (0, (regNew, maxBound))
   where
    validFrame = isJust link

    shiftNew :: BitVector (ShiftRegWidth paw scw)
    shiftNew = resize (shiftOld ++# fromMaybe (deepErrorX "undefined ") link)
    preambleFound = validFrame && resize shiftNew == preamble

    nextState
      | preambleFound = CaptureSequenceCounter
      | otherwise = WaitingForPreamble

    regNew
      | validFrame = Just (shiftNew, resize (pack nextState))
      | otherwise  = Nothing

  go cnt ((shiftOld, unpack . resize -> CaptureSequenceCounter), link, lc) =
    (nextCnt, (regNew, maxBound))
   where
    validFrame = isJust link
    firstFrame = validFrame && cnt == minBound
    lastFrame = validFrame && cnt == maxBound

    withShifted = resize @_ @_ @scw (shiftOld ++# fromMaybe (deepErrorX "undefined ") link)

    shiftNew :: BitVector (ShiftRegWidth paw scw)
    shiftNew
      | firstFrame
      , Dict <- lessThanMax @paw @(scw + scw) @scw
      = setLowerSlice (pack lc ++# withShifted) shiftOld
      | Dict <- lessThanMax @paw @(scw + scw) @scw
      = setLowerSlice withShifted shiftOld

    nextState
      | lastFrame = Idle
      | otherwise = CaptureSequenceCounter

    nextCnt = case oneLTdivRU @scw @fw of
      Dict -> satSucc SatWrap cnt

    regNew
      | validFrame = Just (shiftNew, resize (pack nextState))
      | otherwise  = Nothing

  go _ _ = (0, (Nothing,minBound))

-- | Accepts two 'BitVector's and replaces the lower bits of the second 'BitVector' with
--  the first 'BitVector'.
setLowerSlice ::
  forall slice bv .
  (KnownNat slice, 1 <= slice, KnownNat bv, slice <= bv) =>
  BitVector slice ->
  BitVector bv ->
  BitVector bv
setLowerSlice = setSlice @_ @_ @(bv - slice) (SNat @(slice -1)) d0
