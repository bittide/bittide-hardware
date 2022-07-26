-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Bittide.Link where

import Clash.Prelude

import Bittide.Clash.Extra
import Bittide.Extra.Wishbone
import Bittide.SharedTypes
import Bittide.DoubleBufferedRam
import Data.Constraint
import Data.Constraint.Nat.Extra
import Data.Maybe
import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving

-- Internal states of the txUnit.
data TransmissionState preambleWidth seqCountWidth frameWidth =
  LinkThrough |
  TransmitPreamble (Index (Regs (BitVector preambleWidth) frameWidth)) |
  TransmitSeqCounter (Index (Regs (BitVector seqCountWidth) frameWidth))
   deriving (Generic, NFDataX)

-- | Transmitter for the Bittide Link, it either transmits the incoming gather frame or
-- transmits the preamble followed by the sequence counter.
txUnit ::
  forall core bs aw preambleWidth frameWidth seqCountWidth .
  ( HiddenClockResetEnable core
  , KnownNat preambleWidth
  , KnownNat seqCountWidth
  , KnownNat frameWidth
  , KnownNat bs
  , 1 <= bs
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
  Signal core (WishboneM2S bs aw) ->
  -- |
  -- 1. Control register Wishbone bus (Slave -> master).
  -- 1. Outgoing frame
  ( Signal core (WishboneS2M bs)
  , Signal core (DataLink frameWidth))
txUnit (getRegs -> RegisterBank preamble) sq frameIn wbIn = (wbOut, frameOut)
 where
  stateMachineOn :: Signal core Bool
  (stateMachineOn, wbOut) =
   case timesDivRU @(bs * 8) @1 of
     Dict -> registerWb WishbonePriority False wbIn (pure Nothing)
  frameOut = withReset regReset $ mealy stateMachine (sqErr, LinkThrough) $ bundle (frameIn, sq)
  regReset = orReset stateMachineOn

  stateMachine ::
    (Unsigned seqCountWidth, TransmissionState preambleWidth seqCountWidth frameWidth) ->
    (DataLink frameWidth, Unsigned seqCountWidth) ->
    ( (Unsigned seqCountWidth
    , TransmissionState preambleWidth seqCountWidth frameWidth)
    , DataLink frameWidth)
  stateMachine (sq0@(getRegs -> RegisterBank sqVec),state) (fIn, sqIn) =
   case state of
    LinkThrough -> ((sqErr, TransmitPreamble 0), fIn)
    TransmitPreamble n@((/= maxBound) -> True) -> ((sqErr, TransmitPreamble (succ n)), Just $ preamble !! n)
    TransmitPreamble n -> ((sqIn, TransmitSeqCounter 0), Just $ preamble !! n)
    TransmitSeqCounter n@((/= maxBound) -> True) -> ((sq0, TransmitSeqCounter (succ n)), Just $ sqVec !! n)
    TransmitSeqCounter n -> ((sqErr, TransmitPreamble 0), Just $ sqVec !! n)
  sqErr = deepErrorX "txUnit: Stored sequence counter invalid"

-- | States for the rxUnit.
data ReceiverState =
  Idle |
  WaitingForPreamble |
  CaptureSequenceCounter
  deriving (Generic, ShowX)

-- This annotation makes sure that the state of the receiver can be read as a boolean
-- to check if it is still capturing the sequence counter.
{-# ANN module (DataReprAnn
                  $(liftQ [t|ReceiverState|])
                  2
                  [ ConstrRepr 'Idle                    (2 `downto` 0)  0b00 []
                  , ConstrRepr 'WaitingForPreamble      (2 `downto` 0)  0b01 []
                  , ConstrRepr 'CaptureSequenceCounter  (2 `downto` 0)  0b11 []
                  ]) #-}

deriveBitPack  [t|ReceiverState|]

-- | The width of the internal shift register used for capturing the preamble and two
-- sequence counters.
type ShiftRegWidth paw scw = Max paw (scw + scw)

-- | Receives a Bittide link and can be set to detect the given preamble and capture the
-- following sequence counter.
rxUnit ::
  forall core bs aw paw fw scw .
  ( HiddenClockResetEnable core
  , KnownNat bs, 1 <= bs
  , KnownNat aw, 2 <= aw
  , KnownNat paw, 1 <= paw
  , KnownNat fw, 1 <= fw
  , KnownNat scw, 1 <= scw) =>
  -- | Preamble.
  BitVector paw ->
  -- | Incoming bittide link.
  Signal core (DataLink fw) ->
  -- | Local sequence counter.
  Signal core (Unsigned scw) ->
  -- | Control register Wishbone bus (Master -> slave).
  Signal core (WishboneM2S bs aw) ->
  -- | Control register Wishbone bus (Slave -> master).
  Signal core (WishboneS2M bs)
rxUnit preamble linkIn localCounter wbIn = wbOut
 where
  (regOut, wbOut) = registerWbE WishbonePriority 0 wbIn regIn byteEnables
  (regIn, byteEnables) = unbundle . mealy go initState $ bundle (regOut, linkIn, localCounter)
  initState = 0
  go ::
    Index (DivRU scw fw) ->
    (BitVector (ShiftRegWidth paw scw + bs * 8), DataLink fw, Unsigned scw) ->
    ( Index (DivRU scw fw)
    , (Maybe (BitVector (ShiftRegWidth paw scw + (bs * 8)))
    , ByteEnable (BitVector (ShiftRegWidth paw scw + bs * 8))))
  go _ (split @_ @(ShiftRegWidth paw scw) -> (_, unpack . resize -> Idle), _, _) = (0, (Nothing,minBound))
  go _ (split @_ @(ShiftRegWidth paw scw) -> (shiftOld, unpack . resize -> WaitingForPreamble), link, _) = (0, (regNew, maxBound))
   where
    validFrame = isJust link

    shiftNew :: BitVector (ShiftRegWidth paw scw)
    shiftNew = resize (shiftOld ++# fromMaybe (deepErrorX "undefined ") link)
    preambleFound = validFrame && resize shiftNew == preamble

    nextState
      | preambleFound = CaptureSequenceCounter
      | otherwise = WaitingForPreamble

    regNew
      | validFrame = Just $ shiftNew ++# resize (pack nextState)
      | otherwise  = Nothing

  go cnt (split @_ @(ShiftRegWidth paw scw) -> (shiftOld, unpack . resize -> CaptureSequenceCounter), link, lc) = (nextCnt, (regNew, maxBound))
   where
    validFrame = isJust link
    firstFrame = validFrame && cnt == minBound
    lastFrame = validFrame && cnt == maxBound

    withShifted = resize @_ @_ @scw (shiftOld ++# fromMaybe (deepErrorX "undefined ") link)

    shiftNew :: BitVector (ShiftRegWidth paw scw)
    shiftNew
      | firstFrame = case (lessThanMax @paw @(scw + scw) @scw) of
        Dict -> setLowerSlice (pack lc ++# withShifted) shiftOld
      | otherwise = case (lessThanMax @paw @(scw + scw) @scw) of
        Dict -> setLowerSlice withShifted shiftOld

    nextState
      | lastFrame = Idle
      | otherwise = CaptureSequenceCounter

    nextCnt = case oneLTdivRU @scw @fw of
      Dict -> satSucc SatWrap cnt

    regNew
      | validFrame = Just $ shiftNew ++# resize (pack nextState)
      | otherwise  = Nothing

setLowerSlice :: forall slice bv . (KnownNat slice, KnownNat bv, slice <= bv) =>
  BitVector slice-> BitVector bv -> BitVector bv
setLowerSlice s old = case compareSNat d1 $ SNat @slice of
  SNatLE -> setSlice @(BitVector bv) @(slice - 1) @(bv - slice) @0 SNat SNat s old
  _ -> error "Bittide.Link.setLowerSlice: 1 <= bv - slice does not hold."
