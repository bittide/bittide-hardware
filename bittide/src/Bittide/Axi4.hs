-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Bittide.Axi4 where

import Clash.Prelude

import Protocols
import Protocols.Axi4.Stream

import Bittide.Extra.Maybe

type EndOfPacket = Bool
type BufferFull = Bool

data WbAxisRxBufferState fifoDepth nBytes = WbAxisRxBufferState
  { readingFifo :: Bool
  , packetLength :: Index (fifoDepth * nBytes + 1)
  , writeCounter :: Index fifoDepth
  , packetComplete :: Bool
  , bufferFull :: Bool
  } deriving (Generic, NFDataX)

{-# NOINLINE axisFromByteStream #-}
axisFromByteStream ::
  forall dom dataWidth idWidth destWidth userType .
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , 1 <= dataWidth
  , Eq userType
  , NFDataX userType) =>
  Circuit
    (Axi4Stream dom ('Axi4StreamConfig 1 idWidth destWidth) userType)
    (Axi4Stream dom ('Axi4StreamConfig dataWidth idWidth destWidth) userType)
axisFromByteStream = Circuit (mealyB go initState)
 where
  initState :: (Index dataWidth, Vec (dataWidth - 1) (Unsigned 8, Bool, Bool), userType)
  initState =
    ( 0
    , repeat initTempAxi
    , deepErrorX "axisFromByteStream: Initial user signal is undefined."
    )
  initTempAxi =
    ( 0
    , False
    , False
    )
  go state (Nothing, _) = (state, (Axi4StreamS2M{_tready = True}, Nothing))
  go (oldCounter, storedBytes, prevUser) (Just Axi4StreamM2S{..}, Axi4StreamS2M{_tready}) = ((newCounter, nextStoredBytes, _tuser), (smallS2M, bigM2S))
   where
    smallS2M = Axi4StreamS2M{_tready = smallReady}
    bigM2S
      | not upscaleDone = Nothing
      | otherwise       = Just $ Axi4StreamM2S
        { _tdata = newData
        , _tkeep = newKeeps
        , _tstrb = newStrobes
        , _tuser = if oldCounter > 0 then prevUser else _tuser
        , _tid   = _tid
        , _tdest = _tdest
        , _tlast = _tlast}
    (newData, newKeeps, newStrobes) = unzip3 intermediateBytes

    userValid = oldCounter == 0 || prevUser == _tuser
    upscaleDone = oldCounter == maxBound || not userValid || _tlast
    smallReady  = not upscaleDone || _tready
    newCounter
      | upscaleDone && _tready && _tready = 0
      | upscaleDone = oldCounter
      | otherwise   = satSucc SatWrap oldCounter

    intermediateBytes = replace oldCounter (head _tdata, head _tkeep, head _tstrb)
      (storedBytes :< initTempAxi)

    nextStoredBytes
      | upscaleDone && _tready = repeat initTempAxi
      | otherwise              = init intermediateBytes

{-# NOINLINE axisToByteStream #-}
axisToByteStream ::
  forall dom dataWidth idWidth destWidth userType .
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , 1 <= dataWidth
  , Eq userType
  , NFDataX userType) =>
  Circuit
    (Axi4Stream dom ('Axi4StreamConfig dataWidth idWidth destWidth) userType)
    (Axi4Stream dom ('Axi4StreamConfig 1 idWidth destWidth) userType)
axisToByteStream = Circuit (mealyB go (0 :: Index dataWidth))
 where
  go state (Nothing, _) = (state, (Axi4StreamS2M{_tready = True}, Nothing))
  go oldCounter (Just Axi4StreamM2S{..}, Axi4StreamS2M{_tready}) = (newCounter, (bigS2M, smallM2S))
   where
    bigS2M = Axi4StreamS2M{_tready = _tready && lastByte}
    lastByte = oldCounter == lastValidByteIndex
    smallM2S = Just $ Axi4StreamM2S
        { _tdata = outData :> Nil
        , _tkeep = outKeep :> Nil
        , _tstrb = outStrb :> Nil
        , _tuser = _tuser
        , _tid   = _tid
        , _tdest = _tdest
        , _tlast = _tlast && lastByte}
    (outData, outKeep, outStrb) = zip3 _tdata _tkeep _tstrb !! oldCounter
    lastValidByteIndex = fromMaybesR 0 (orNothing <$> _tkeep <*> indicesI)
    newCounter
      | _tready && lastByte = 0
      | _tready = satSucc SatWrap oldCounter
      | otherwise = oldCounter
