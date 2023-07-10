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


{-# NOINLINE axisFromByteStream #-}
-- | Transforms an Axi4 stream of 1 byte wide into an Axi4 stream of /n/ bytes
-- wide. If it encounters '_tlast' or has captured /n/ bytes, it will present
-- the transaction at the output. Note that if less than /n/ bytes have been
-- captured, but '_tlast' is set, the component will immediately output the
-- captured bytes with appropriately set '_tkeep' bits. The '_tuser', _tdest'
-- and '_tid' signals are blindly routed to the output. This effectively means
-- that all but the last '_tuser', '_tdest', '_tid' are linked to a valid
-- transaction.
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
  initState :: (Index dataWidth, Vec (dataWidth - 1) (Unsigned 8, Bool, Bool))
  initState = (0, repeat initTempAxi)
  initTempAxi = (0, False, False)
  go state (Nothing, _) = (state, (Axi4StreamS2M{_tready = True}, Nothing))
  go (counter, storedBytes) (Just Axi4StreamM2S{..}, Axi4StreamS2M{_tready}) =
    ((newCounter, nextStoredBytes), (smallS2M, bigM2S))
   where
    smallS2M = Axi4StreamS2M{_tready = smallReady}
    bigM2S
      | not upscaleDone = Nothing
      | otherwise       = Just $ Axi4StreamM2S
        { _tdata = newData
        , _tkeep = newKeeps
        , _tstrb = newStrobes
        , _tuser = _tuser
        , _tid   = _tid
        , _tdest = _tdest
        , _tlast = _tlast}
    (newData, newKeeps, newStrobes) = unzip3 intermediateBytes

    upscaleDone = counter == maxBound || _tlast
    smallReady  = not upscaleDone || _tready
    newCounter
      | upscaleDone && _tready && _tready = 0
      | upscaleDone = counter
      | otherwise   = satSucc SatWrap counter

    -- TODO: Replace this relatively expensive replace operation with a shift operation
    intermediateBytes = replace counter (head _tdata, head _tkeep, head _tstrb)
      (storedBytes :< initTempAxi)

    nextStoredBytes
      | upscaleDone && _tready = repeat initTempAxi
      | otherwise              = init intermediateBytes

{-# NOINLINE axisToByteStream #-}
-- | Transforms an Axi4 stream of /n/ bytes wide into an Axi4 stream of 1 byte
-- wide. It uses an internal counter to select the bytes of the incoming axi
-- stream one by one. The incoming stream is acknowledged when the last byte is
-- acknowledged by the outgoing stream. The '_tuser', '_tdest' and '_tid' are
-- blindly routed to the output.
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
  go counter (Just Axi4StreamM2S{..}, Axi4StreamS2M{_tready}) = (newCounter, (bigS2M, smallM2S))
   where
    bigS2M = Axi4StreamS2M{_tready = _tready && lastByte}
    lastByte = counter == lastValidByteIndex
    smallM2S = Just $ Axi4StreamM2S
        { _tdata = outData :> Nil
        , _tkeep = outKeep :> Nil
        , _tstrb = outStrb :> Nil
        , _tuser = _tuser
        , _tid   = _tid
        , _tdest = _tdest
        , _tlast = _tlast && lastByte}
    -- TODO: The use of !! is typically rather expensive because it produces an n-mux
    -- Look into optimizing by loading the n bytes in a register and shifting them out
    -- byte by byte
    (outData, outKeep, outStrb) = zip3 _tdata _tkeep _tstrb !! counter
    lastValidByteIndex = fromMaybesR 0 (orNothing <$> _tkeep <*> indicesI)
    newCounter
      | _tready && lastByte = 0
      | _tready = satSucc SatWrap counter
      | otherwise = counter
