-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=6 #-}
module Bittide.Link where

import Clash.Prelude

import Bittide.Extra.Wishbone
import Bittide.SharedTypes
import Bittide.DoubleBufferedRam
import Data.Constraint
import Data.Constraint.Nat.Extra

data TransmissionState preambleWidth seqCountWidth frameWidth =
  LinkThrough |
  TransmitPreamble (Index (Regs (BitVector preambleWidth) frameWidth)) |
  TransmitSeqCounter (Index (Regs (BitVector seqCountWidth) frameWidth))
   deriving (Generic, NFDataX)

txUnit ::
  forall core bytes addrWidth preambleWidth frameWidth seqCountWidth.
  ( HiddenClockResetEnable core
  , KnownNat preambleWidth
  , 1 <= preambleWidth
  , KnownNat seqCountWidth
  , 1 <= seqCountWidth
  , KnownNat frameWidth
  , KnownNat bytes
  , 1 <= bytes
  , KnownNat addrWidth
  , 2 <= addrWidth
  , 1 <= frameWidth) =>
  -- |  Hardcoded preamble.
  BitVector preambleWidth ->
  -- | Local sequence counter.
  Signal core (BitVector seqCountWidth) ->
  -- | Control register wishbone bus (Master to slave).
  Signal core (WishboneM2S bytes addrWidth) ->
  -- | Frame from 'gatherUnitWb'
  Signal core (DataLink frameWidth) ->
  -- |
  -- 1. Control register wishbone bus (Slave to master).
  -- 1. Outgoing frame
  ( Signal core (WishboneS2M bytes)
  , Signal core (DataLink frameWidth))
txUnit (getRegs -> RegisterBank preamble) sq wbIn frameIn = (wbOut, frameOut)
 where
  stateMachineOn :: Signal core Bool
  (stateMachineOn, wbOut) =
   case timesDivRU @(bytes * 8) @1 of
     Dict -> registerWb WishbonePriority False wbIn (pure Nothing)
  frameOut = withReset regReset $ mealy stateMachine LinkThrough $ bundle (frameIn, sq)
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

shiftRegister ::
  forall dom bitsIn bitsOut .
  (HiddenClockResetEnable dom, KnownNat bitsIn, KnownNat bitsOut) =>
  Signal dom (BitVector bitsIn) ->
  Signal dom (BitVector bitsOut)
shiftRegister bvIn = bvOut
 where
   bvOut = register 0 $ (\ a b -> truncateB (a ++# b)) <$> bvOut <*> bvIn
