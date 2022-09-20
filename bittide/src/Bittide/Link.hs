-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A unidirectional communication primitive tht moves a fixed-rate stream of frames
-- between a pair of nodes. The frame size can be unique for each link including the possibility
-- of single bit frames. The links and infrastructure perform zero in-band signaling.
-- A link starts with a 'gatherUnit' and 'txUnit' and is terminated by a 'rxUnit' and
-- 'scatterUnit'. A 'Bittide.Switch' contains single depth versions of the 'gatherUnit'
-- and 'scatterUnit' that essentially reduce to a single 'register'.
module Bittide.Link where

import Clash.Prelude

import Data.Constraint
import Data.Constraint.Nat.Extra
import Data.Maybe
import Protocols.Wishbone

import Bittide.DoubleBufferedRam
import Bittide.ScatterGather
import Bittide.SharedTypes

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
  Signal core (WishboneM2S aw nBytes (Bytes nBytes)) ->
  -- |
  -- 1. Control register Wishbone bus (Slave -> master).
  -- 2. Outgoing frame
  ( Signal core (WishboneS2M (Bytes nBytes))
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
  Signal core (WishboneM2S aw nBytes (Bytes nBytes)) ->
  -- | Control register Wishbone bus (Slave -> master).
  Signal core (WishboneS2M (Bytes nBytes))
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

-- | Configuration for a 'Bittide.Link.
data LinkConfig nBytes addrW where
  LinkConfig ::
    (KnownNat preambleWidth, 1 <= preambleWidth) =>
    -- | Preamble for 'txUnit' and 'rxUnit'.
    BitVector preambleWidth ->
    -- | Configuration for the receiving 'scatterUnitWb'.
    ScatterConfig nBytes addrW ->
    -- | Configuration for the transmitting 'gatherUnitWb'
    GatherConfig nBytes addrW ->
    LinkConfig nBytes addrW

-- | Offers interfaces to connect an incoming 'Bittide.Link' to a 'processingElement',
--  consists of a 'rxUnit' and 'scatterUnitWb'. The busses for the
-- 'scatterUnitWb's 'calendar' and 'rxUnit' are exposed for the 'managementUnit',
-- the bus for the 'scatterUnitWb's memory interface is exposed for the 'processingElement'.
linkToPe ::
  forall dom scw nBytesMu addrWMu addrWPe .
  ( HiddenClockResetEnable dom
  , KnownNat nBytesMu, 1 <= nBytesMu
  , KnownNat addrWMu, 2 <= addrWMu
  , KnownNat addrWPe, 2 <= addrWPe
  , KnownNat scw, 1 <= scw)=>
  -- | Configuration for a 'Bittide.Link', the receiving end uses this for its @preamble@
  -- and 'ScatterConfig'.
  LinkConfig nBytesMu addrWMu ->
  -- | Incoming 'Bittide.Link'.
  Signal dom (DataLink 64) ->
  -- | Input for local 'sequenceCounter'.
  Signal dom (Unsigned scw) ->
  -- | Master input for the 'scatterUnitWb's memory interface.
  Signal dom (WishboneM2S addrWPe 4 (Bytes 4)) ->
  -- | Master bus for the 'rxUnit' and the 'scatterUnitWb's 'calendar', respectively.
  Vec 2 (Signal dom (WishboneM2S addrWMu nBytesMu (Bytes nBytesMu))) ->
  -- |
  --   ( Slave output for the 'scatterUnitWb's memory interface
  --   , Slave outputs for the 'rxUnit' and 'scatterUnitWb's 'calendar', respectively)
  ( Signal dom (WishboneS2M (Bytes 4))
  , Vec 2 (Signal dom (WishboneS2M (Bytes nBytesMu))))
linkToPe linkConfig linkIn localCounter peM2S linkM2S = case linkConfig of
  LinkConfig preamble scatConfig _ -> (peS2M, linkS2M)
   where
    linkS2M =  rxS2M :> calS2M :> Nil
    (rxM2S :> calM2S :> Nil) = linkM2S
    rxS2M = rxUnit preamble localCounter linkIn rxM2S
    (peS2M,calS2M) = scatterUnitWb scatConfig calM2S linkIn peM2S


-- | Offers interfaces to connect an outgoing 'Bittide.Link' to a 'processingElement',
--  consists of a 'txUnit' and 'gatherUnitWb'. The busses for the
-- 'gatherUnitWb's 'calendar' and 'txUnit' are exposed for the 'managementUnit',
-- the bus for the 'gatherUnitWb's memory interface is exposed for the 'processingElement'.
peToLink ::
  forall dom scw nBytesMu addrWMu addrWPe .
  ( HiddenClockResetEnable dom
  , KnownNat nBytesMu, 1 <= nBytesMu
  , KnownNat addrWMu, 2 <= addrWMu
  , KnownNat addrWPe, 2 <= addrWPe
  , KnownNat scw, 1 <= scw) =>
  -- | Configuration for a 'Bittide.Link', the transmitting end uses this for its @preamble@
  -- and 'GatherConfig'.
  LinkConfig nBytesMu addrWMu ->
  -- | Input for local 'sequenceCounter'.
  Signal dom (Unsigned scw) ->
  -- | Master input for the 'gatherUnitWb's memory interface.
  Signal dom (WishboneM2S addrWPe 4 (Bytes 4)) ->
  -- | Master bus for the 'txUnit' and the 'gatherUnitWb's 'calendar', respectively.
  Vec 2 (Signal dom (WishboneM2S addrWMu nBytesMu (Bytes nBytesMu))) ->
  -- |
  --   ( Outgoing 'Bittide.Link'
  --   , Slave output for the 'gatherUnitWb's memory interface
  --   , Slave outputs for the 'txUnit' and 'gatherUnitWb's 'calendar', respectively)
  ( Signal dom (DataLink 64)
  , Signal dom (WishboneS2M (Bytes 4))
  , Vec 2 (Signal dom (WishboneS2M (Bytes nBytesMu))))
peToLink linkConfig localCounter peM2S linkM2S = case linkConfig of
  LinkConfig preamble _ gathConfig -> go preamble gathConfig
 where
  go ::
    forall preambleWidth .
    ( KnownNat preambleWidth, 1 <= preambleWidth) =>
    BitVector preambleWidth ->
    GatherConfig nBytesMu addrWMu ->
    ( Signal dom (DataLink 64)
    , Signal dom (WishboneS2M (Bytes 4))
    , Vec 2 (Signal dom (WishboneS2M (Bytes nBytesMu))))
  go preamble calConfig = (linkOut, peS2M, linkS2M)
   where
    linkS2M =  txS2M :> calS2M :> Nil
    (txM2S :> calM2S :> Nil) = linkM2S
    (txS2M,linkOut) = txUnit preamble localCounter gatherOut txM2S
    (gatherOut, peS2M,calS2M) = gatherUnitWb calConfig calM2S peM2S

-- | Counts the number of cycles since the last reset. Initially Unsigned 64 has been
--  picked because it's unlikely to overflow in the lifetime of a Bittide system.
sequenceCounter :: HiddenClockResetEnable dom => Signal dom (Unsigned 64)
sequenceCounter = register 0 $ satSucc SatError <$> sequenceCounter
