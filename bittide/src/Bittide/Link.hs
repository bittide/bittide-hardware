-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=6 #-}
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
  frameOut = withReset regReset $ mealy stateMachine LinkThrough $ bundle (frameIn, pack <$> sq)
  regReset = orReset stateMachineOn

  stateMachine ::
    TransmissionState preambleWidth seqCountWidth frameWidth->
    (DataLink frameWidth, BitVector seqCountWidth) ->
    ( TransmissionState preambleWidth seqCountWidth frameWidth
    , DataLink frameWidth)
  stateMachine state (fIn, getRegs -> RegisterBank sqIn) =
   case state of
    LinkThrough -> (TransmitPreamble 0, fIn)
    (TransmitPreamble n@((== maxBound) -> False)) -> (TransmitPreamble (n+1), Just $ preamble !! n)
    (TransmitPreamble n@((== maxBound) -> True)) -> (TransmitSeqCounter 0, Just $ preamble !! n)
    (TransmitSeqCounter n@((== maxBound) -> False)) -> (TransmitSeqCounter (n+1), Just $ sqIn !! n)
    (TransmitSeqCounter n@((== maxBound) -> True)) -> (TransmitPreamble 0, Just $ sqIn !! n)

rxUnit ::
  forall core bs aw preambleWidth frameWidth seqCountWidth .
  ( HiddenClockResetEnable core
  , KnownNat bs, 1 <= bs
  , KnownNat aw, 2 <= aw
  , KnownNat preambleWidth, 1 <= preambleWidth
  , KnownNat frameWidth, 1 <= frameWidth
  , KnownNat seqCountWidth, 1 <= seqCountWidth) =>
  Proxy seqCountWidth ->
  BitVector preambleWidth ->
  Signal core (WishboneM2S bs aw) ->
  Signal core (DataLink frameWidth) ->
  Signal core (WishboneS2M bs)
rxUnit Proxy preamble wbIn linkIn = wbOut
 where
  shiftReg :: Signal core (BitVector (Max preambleWidth seqCountWidth))
  (shiftReg, wbOut) = case lessThanMax @preambleWidth @seqCountWidth @1 of
    Dict -> rxShiftRegister wbIn linkIn stopSignal
  preambleFound = (==preamble) . (resize @_ @_ @preambleWidth) <$> shiftReg
  stopSignal = captureCounter .==. pure (maxBound :: Index (DivRU seqCountWidth frameWidth))
  captureCounter = andEnable (preambleFound .||. (/=0) <$> captureCounter) register 0 $ succ <$> captureCounter

rxShiftRegister ::
  forall dom bs aw shiftRegSize frameWidth .
  ( HiddenClockResetEnable dom
  , KnownNat bs, 1 <= bs
  , KnownNat aw, 2 <= aw
  , KnownNat shiftRegSize, 1 <= shiftRegSize
  , KnownNat frameWidth) =>
  Signal dom (WishboneM2S bs aw) ->
  Signal dom (DataLink frameWidth) ->
  Signal dom Bool ->
  (Signal dom (BitVector shiftRegSize), Signal dom (WishboneS2M bs))
rxShiftRegister wbIn shiftIn stopShifting = (shiftOut, wbOut)
 where
  regOut :: Signal dom (BitVector (shiftRegSize + bs * 8))
  (regOut, wbOut) = registerWbE WishbonePriority 0 wbIn regIn byteEnables
  (regIn, byteEnables, shiftOut) =
    case sameNat
      (Proxy @(DivRU shiftRegSize 8 + bs))
      (Proxy @(DivRU (shiftRegSize + bs * 8) 8)) of
    Just Refl -> unbundle $ go <$> shiftIn <*> regOut <*> stopShifting

  go ::
    Maybe (BitVector frameWidth) ->
    BitVector (shiftRegSize + bs * 8) ->
    Bool ->
    ( Maybe (BitVector (shiftRegSize + bs * 8))
    , BitVector (DivRU shiftRegSize 8 + bs)
    , BitVector shiftRegSize)
  go shiftIn0 regOut0 stopShifting0 = (regIn0, byteEnables0, oldShifted)
   where
    (oldShifted, reg0) = split regOut0
    (_, unpack -> shiftEnable0) = split @_ @(bs * 8 - 1) reg0
    newShifted = truncateB @_ @shiftRegSize (oldShifted ++# fromJust shiftIn0)
    regIn0 = Just $ newShifted ++# (0 :: BitVector (bs * 8))
    shiftEnables
      | shiftEnable0 && isJust shiftIn0 = maxBound @(BitVector (DivRU shiftRegSize 8))
      | otherwise = 0
    byteEnables0 = shiftEnables ++# (0 :: BitVector (bs -1)) ++# pack stopShifting0

{-# NOINLINE rxShiftRegister #-}
