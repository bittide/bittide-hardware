-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
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
import Data.Proxy
import Data.Type.Equality ((:~:)(Refl))

data TransmissionState preambleWidth seqCountWidth frameWidth =
  LinkThrough |
  TransmitPreamble (Index (Regs (BitVector preambleWidth) frameWidth)) |
  TransmitSeqCounter (Index (Regs (BitVector seqCountWidth) frameWidth))
   deriving (Generic, NFDataX)

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
  -- | Control register wishbone bus (Master to slave).
  Signal core (WishboneM2S bs aw) ->
  -- | Frame from 'gatherUnitWb'
  Signal core (DataLink frameWidth) ->
  -- |
  -- 1. Control register wishbone bus (Slave to master).
  -- 1. Outgoing frame
  ( Signal core (WishboneS2M bs)
  , Signal core (DataLink frameWidth))
txUnit (getRegs -> RegisterBank preamble) sq wbIn frameIn = (wbOut, frameOut)
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
    TransmitPreamble n@((== maxBound) -> False) -> ((sqErr, TransmitPreamble (succ n)), Just $ preamble !! n)
    TransmitPreamble n@((== maxBound) -> True) -> ((sqIn, TransmitSeqCounter 0), Just $ preamble !! n)
    TransmitSeqCounter n@((== maxBound) -> False) -> ((sq0, TransmitSeqCounter (succ n)), Just $ sqVec !! n)
    TransmitSeqCounter n@((== maxBound) -> True) -> ((sqErr, TransmitPreamble 0), Just $ sqVec !! n)
  sqErr = deepErrorX "txUnit: Stored sequence counter invalid"

type ShiftRegWidth paw scw = Max paw scw
type RxRegWidth paw scw bs = Max (scw + scw) (ShiftRegWidth paw scw) + bs * 8
type ShiftReg paw scw = BitVector (ShiftRegWidth paw scw)

rxUnit ::
  forall core bs aw preambleWidth frameWidth seqCountWidth .
  ( HiddenClockResetEnable core
  , KnownNat bs, 1 <= bs
  , KnownNat aw, 2 <= aw
  , KnownNat preambleWidth, 1 <= preambleWidth
  , KnownNat frameWidth, 1 <= frameWidth
  , KnownNat seqCountWidth, 1 <= seqCountWidth) =>
  BitVector preambleWidth ->
  Signal core (WishboneM2S bs aw) ->
  Signal core (DataLink frameWidth) ->
  Signal core (Unsigned seqCountWidth) ->
  Signal core (WishboneS2M bs)
rxUnit preamble wbIn linkIn localCounter = wbOut
 where
  (shiftReg, wbOut) = case lessThanMax @preambleWidth @seqCountWidth @1 of
    Dict -> rxRegister (Proxy @preambleWidth) wbIn linkIn stopSignal localCounter
  preambleFound = (==preamble) . (resize @_ @(ShiftRegWidth preambleWidth seqCountWidth) @preambleWidth)  <$> shiftReg
  stopSignal = captureCounter .==. pure (maxBound :: Index (DivRU seqCountWidth frameWidth))
  captureCounter = andEnable (preambleFound .||. (/=0) <$> captureCounter) register 0 $ succ <$> captureCounter

rxRegister ::
  forall dom bs aw paw scw fw .
  ( HiddenClockResetEnable dom
  , KnownNat bs, 1 <= bs
  , KnownNat aw, 2 <= aw
  , KnownNat paw, 1 <= paw
  , KnownNat scw, 1 <= scw
  , KnownNat fw) =>
  Proxy paw ->
  Signal dom (WishboneM2S bs aw) ->
  Signal dom (DataLink fw) ->
  Signal dom Bool ->
  Signal dom (Unsigned scw) ->
  (Signal dom (ShiftReg paw scw), Signal dom (WishboneS2M bs))
rxRegister Proxy wbIn shiftIn stopShifting localCounter = (shiftOut, wbOut)
 where
  (regOut, wbOut) = registerWbE WishbonePriority 0 wbIn regIn byteEnables
  (regIn, byteEnables, shiftOut) =
    unbundle $ go <$> shiftIn <*> regOut <*> stopShifting <*> localCounter

  go ::
    Maybe (BitVector fw) ->
    BitVector (RxRegWidth paw scw bs) ->
    Bool ->
    Unsigned scw ->
    ( Maybe (BitVector (RxRegWidth paw scw bs))
    , BitVector (DivRU (RxRegWidth paw scw bs) 8)
    , ShiftReg paw scw)
  go shiftIn0 regOut0 stopShifting0 lc0 = (regIn0 , byteEnables0, oldShifted)
   where
    (resize -> oldShifted, unpack . resize -> shiftEnable) = split @_ @_ @(bs * 8) regOut0
    newShifted = resize @_ @_ @(ShiftRegWidth paw scw) (oldShifted ++# fromJust shiftIn0)
    bothCounters = pack lc0 ++# resize newShifted :: BitVector (scw + scw)
    bvLow = 0 :: Bytes bs
    baseInput :: BitVector (Max (scw + scw) (ShiftRegWidth paw scw))
    baseInput = case (sameNat @(scw + scw) @(Max (scw + scw) (ShiftRegWidth paw scw)) SNat SNat , sameNat @(ShiftRegWidth paw scw) @(Max (scw + scw) (ShiftRegWidth paw scw)) SNat  SNat) of
      (Just Refl,Nothing) -> bothCounters
      (Nothing, Just Refl) -> newShifted
      _ -> error "Bittide.Link.rxRegister: Register size does not match either the preamble width or twice the sequence counter width."

    regIn0 :: Maybe (BitVector  (RxRegWidth paw scw bs))
    regIn0 = case
      (shiftEnable, isJust shiftIn0, stopShifting0) of
        (True, True, False) -> case lessThanMax @(scw + scw) @(Max paw scw) @(Max paw scw) of
          Dict -> Just $ setLowerSlice newShifted baseInput ++# bvLow
        (True, True, True) ->
          Just $ setLowerSlice bothCounters baseInput ++# bvLow
        _ -> Nothing

    enables :: BitVector (DivRU (Max (scw + scw) (Max paw scw)) 8)
    enables
      | stopShifting0 && isJust shiftIn0 = case lessThanDivRUMax @(scw + scw) @(Max paw scw) @8 of
        Dict -> setLowerSlice (maxBound :: BitVector (DivRU (scw + scw) 8)) 0
      | isJust shiftIn0 = case lessThanDivRUMax @(scw + scw) @(Max paw scw) @8 of
        Dict -> setLowerSlice (maxBound :: BitVector (DivRU (ShiftRegWidth paw scw) 8)) 0
      | otherwise = 0
    byteEnables0 = case sameNat @(DivRU (RxRegWidth paw scw bs) 8) @(DivRU (Max (scw + scw) (Max paw scw)) 8 + bs) SNat SNat of
      Just Refl -> enables ++# (0 :: BitVector (bs -1)) ++# pack stopShifting0

{-# NOINLINE rxRegister #-}

setLowerSlice :: forall slice bv . (KnownNat slice, KnownNat bv, slice <= bv) =>
  BitVector slice-> BitVector bv -> BitVector bv
setLowerSlice s old = case compareSNat d1 $ SNat @(bv - slice) of
  SNatLE -> slice @_ @(bv - slice-1) @_ @0 SNat SNat old ++# s
  _ -> error "Bittide.Link.setLowerSlice: 1 <= bv - slice does not hold."
