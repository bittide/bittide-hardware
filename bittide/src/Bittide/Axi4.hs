-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}

module Bittide.Axi4 where

import Clash.Prelude

import Clash.Sized.Internal.BitVector (popCountBV)
import Data.Maybe
import Data.Constraint
import Protocols
import Protocols.Axi4.Stream
import Protocols.Wishbone
import Protocols.Df hiding (const, pure, stall)
import Protocols.Internal

import Bittide.Extra.Maybe
import Bittide.SharedTypes
import Data.Constraint.Nat.Extra

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

{-# NOINLINE wbAxisRxBuffer #-}

wbAxisRxBufferCircuit ::
  forall dom wbAddrW nBytes fifoDepth conf .
  ( HiddenClockResetEnable dom
  , KnownNat wbAddrW, 2 <= wbAddrW
  , KnownNat nBytes, 1 <= nBytes
  , 1 <= fifoDepth
  , 1 <= nBytes * fifoDepth
  , DataWidth conf ~ nBytes) =>
  -- | Depth of the buffer, each entry in the buffer stores `nBytes` bytes.
  SNat fifoDepth ->
  Circuit
    (Wishbone dom 'Standard wbAddrW (Bytes nBytes), Axi4Stream dom conf ())
    (CSignal dom (EndOfPacket, BufferFull))
wbAxisRxBufferCircuit depth = case cancelMulDiv @nBytes @8 of
  Dict -> Circuit $ \((wbM2S, axiM2S),_) -> do
    let (wbS2M, axiS2M,status) = wbAxisRxBuffer depth wbM2S axiM2S $ pure (False, False)
      in ((wbS2M, axiS2M), CSignal status)
-- | A wishbone accessible buffer that stores AXI4Stream packets. The buffer stores
-- a single Axi4 stream packet and exposes a status register that indicates:
--  * If the buffer contains a packet
--  * If the buffer is full before, but does not contain a whole packet.
--
-- The wishbone addressing must be 4 byte aligned and is as follows:
--  * 0 .. 4 * (fifoDepth - 1) = Read-only access into the buffer.
--  * 4 * fifoDepth            = Byte count register.
--  * 4 * (fifoDepth + 1)      = Status register
--
-- After reading a packet, the byte count must be set to 0 and the status register must be
-- cleared.
--
-- The external status clear signals clear on True, set to (False, False) if not used.
wbAxisRxBuffer ::
  forall dom wbAddrW nBytes fifoDepth conf axiUserType .
  ( HiddenClockResetEnable dom
  , KnownNat wbAddrW, 2 <= wbAddrW
  , KnownNat nBytes, 1 <= nBytes
  , 1 <= fifoDepth
  , 1 <= nBytes * fifoDepth
  , DataWidth conf ~ nBytes) =>
  -- | Depth of the buffer, each entry in the buffer stores `nBytes` bytes.
  SNat fifoDepth ->
  -- | Wishbone master bus.
  "wbM2S" ::: Signal dom (WishboneM2S wbAddrW nBytes (Bytes nBytes)) ->
  -- | Axi4 Stream master bus.
  "axisM2S" ::: Signal dom (Maybe (Axi4StreamM2S conf axiUserType)) ->
  -- | External controls to clear bits in the status register.
  "clearinterrupts" ::: Signal dom (EndOfPacket, BufferFull) ->
  -- |
  -- 1. Wishbone slave bus
  -- 2. Axi4 Stream slave bus
  -- 3. Status
  "" :::
  ( "wbS2M" ::: Signal dom (WishboneS2M (Bytes nBytes))
  , "axisS2M" ::: Signal dom Axi4StreamS2M
  , "status" ::: Signal dom (EndOfPacket, BufferFull)
  )
wbAxisRxBuffer SNat wbM2S axisM2S statusClearSignal = (wbS2M, axisS2M, statusReg)
 where
  fifoOut =
    blockRamU NoClearOnReset (SNat @fifoDepth)
    (const $ errorX "wbAxisRxBuffer: reset function undefined")
    bramAddr bramWrite
  (wbS2M, axisS2M, bramAddr, bramWrite, statusReg) =
    mealyB go initState (wbM2S, axisM2S, fifoOut, statusClearSignal)
  initState = WbAxisRxBufferState
    { readingFifo = False
    , packetLength = 0
    , writeCounter = 0
    , packetComplete = False
    , bufferFull = False
    }
  go ::
    WbAxisRxBufferState fifoDepth nBytes ->
    ( WishboneM2S wbAddrW nBytes (Bytes nBytes)
    , Maybe (Axi4StreamM2S conf axiUserType)
    , Bytes nBytes, (EndOfPacket, BufferFull)
    ) ->
    ( WbAxisRxBufferState fifoDepth nBytes
    , ( WishboneS2M (Bytes nBytes)
      , Axi4StreamS2M, Index fifoDepth
      , Maybe (Index fifoDepth, Bytes nBytes)
      , (EndOfPacket, BufferFull)
      )
    )
  go
    WbAxisRxBufferState{..}
    (WishboneM2S{..}, maybeAxisM2S, wbData, clearStatus)
    = (newState, output)
   where
    masterActive = busCycle && strobe
    (alignedAddress, alignment) = split @_ @(wbAddrW - 2) @2 addr

    packetLengthAddress = maxBound - 1
    statusAddress       = maxBound :: Index (fifoDepth + 2)
    internalAddress = (bitCoerce $ resize alignedAddress) :: Index (fifoDepth + 2)

    err = masterActive && (alignment /= 0 || alignedAddress > resize (pack statusAddress))

    statusBV = pack (packetComplete, bufferFull)
    wbHandshake = masterActive && not err

    (readData, nextReadingFifo, wbAcknowledge)
      | masterActive && internalAddress == packetLengthAddress = (resize $ pack packetLength, False, wbHandshake)
      | masterActive && internalAddress == statusAddress       = (resize statusBV, False, wbHandshake)
      | otherwise                                              = (wbData, wbHandshake && not readingFifo, wbHandshake && readingFifo)

    axisReady = not (packetComplete || bufferFull)
    axisHandshake = axisReady && isJust maybeAxisM2S

    output =
      ( (emptyWishboneS2M @(Bytes nBytes)){readData, err, acknowledge = wbAcknowledge}
      , Axi4StreamS2M axisReady
      , resize internalAddress
      , ((writeCounter,) . pack . reverse . _tdata <$> maybeAxisM2S) >>= orNothing axisHandshake
      , (packetComplete, bufferFull)
      )

    -- Next state
    (nextPacketComplete, nextBufferFull)
      | wbAcknowledge && writeEnable && internalAddress == statusAddress
        = unpack $ (\ a b -> a `xor` (a .&. b)) statusBV (resize writeData)
      | axisHandshake = (packetComplete || maybe False _tlast maybeAxisM2S, bufferFull || writeCounter == maxBound)
      | otherwise = unpack $ statusBV `xor` pack clearStatus

    nextWriteCounter
      | axisHandshake  = satSucc SatError writeCounter
      | packetComplete = 0
      | otherwise      = writeCounter

    bytesInStream = maybe (0 :: Index (nBytes + 1)) (leToPlus @1 @nBytes popCountBV . pack ._tkeep) maybeAxisM2S

    nextPacketLength
      | wbAcknowledge && writeEnable && internalAddress == packetLengthAddress = unpack $ resize writeData
      | axisHandshake                                         = satAdd SatBound packetLength (bitCoerce $ resize bytesInStream)
      | otherwise                                             = packetLength

    newState = WbAxisRxBufferState
      { readingFifo = nextReadingFifo
      , packetLength = nextPacketLength
      , writeCounter = nextWriteCounter
      , packetComplete = nextPacketComplete
      , bufferFull = nextBufferFull
      }

type AxiStreamBytesOnly nBytes = 'Axi4StreamConfig nBytes 0 0
wbToAxiTx ::
  forall dom addrW nBytes .
 ( KnownNat addrW
 , 2 <= addrW
 , KnownNat nBytes) =>
  Circuit
    (Wishbone dom 'Standard addrW (Bytes nBytes))
    (Axi4Stream dom (AxiStreamBytesOnly nBytes) ())
wbToAxiTx = case cancelMulDiv @nBytes @8 of
  Dict -> Circuit $ unbundle . fmap go . bundle
 where
  go (WishboneM2S{..}, Axi4StreamS2M{..}) =
    (WishboneS2M{readData, err, acknowledge, retry, stall}, axiM2S)
   where
    (internalAddress, alignment) = split @_ @(addrW -2) @2 addr
    masterActive = busCycle && strobe
    addrValid = shiftR internalAddress 1 == 0 && alignment == 0
    err = masterActive && not (addrValid && writeEnable)
    acknowledge = masterActive && not err && _tready
    readData = deepErrorX "wbToAxiStream: readData undefined" :: Bytes nBytes
    retry = False
    stall = False

    (_tkeep, _tlast)
      | lsb internalAddress == 0 = (reverse $ unpack busSelect, False)
      | otherwise                = (repeat False, True)

    _tstrb = repeat True
    _tid = deepErrorX "wbToAxiStream: _tid undefined"
    _tdest = deepErrorX "wbToAxiStream: _tdest undefined"
    _tuser = ()
    _tdata = reverse $ unpack writeData
    axiM2S :: Maybe (Axi4StreamM2S (AxiStreamBytesOnly nBytes) ())
    axiM2S = orNothing (masterActive && not err)
      Axi4StreamM2S{_tdata, _tkeep, _tstrb, _tlast, _tid, _tdest, _tuser}

axiPayloadFifo ::
  ( HiddenClockResetEnable dom
  , KnownNat fifoDepth, 1 <= fifoDepth
  , KnownNat nBytes) =>
  SNat fifoDepth ->
  Circuit
    (Axi4Stream dom (AxiStreamBytesOnly nBytes) ())
    (Axi4Stream dom (AxiStreamBytesOnly nBytes) ())
axiPayloadFifo fifoDepth = Circuit go
 where
  go (axiLeftM, axiRightS) = ((\ (Ack b) -> Axi4StreamS2M b) <$> dfS, axiRightM)
   where
    axiRightM = liftA2 (\ axi newData -> axi{_tdata = newData}) <$> axiLeftM <*> fmap dataToMaybe dfM
    (dfS, dfM) = toSignals (fifo fifoDepth) (maybeToData . fmap _tdata <$> axiLeftM, (Ack . _tready) <$> axiRightS)

axiStreamPacketFifo ::
  ( HiddenClockResetEnable dom
  , KnownNat fifoDepth, 2 <= fifoDepth
  , KnownNat nBytes) =>
  SNat fifoDepth ->
  Circuit
    (Axi4Stream dom (AxiStreamBytesOnly nBytes) ())
    (Axi4Stream dom (AxiStreamBytesOnly nBytes) ())
axiStreamPacketFifo fifoDepth = Circuit goCircuit
 where
  goCircuit (axiInM2S, axiOutS2M) = (Axi4StreamS2M <$> consumeAxiIn, axiOutM2S)
   where
    axiOutM2S = mux consumeFifo fifoOut (pure Nothing)
    fifoIn    = mux consumeAxiIn axiInM2S (pure Nothing)
    (consumeFifo, consumeAxiIn) = mealyB goMealy False (fifoOut, fifoReady, axiInM2S)
    (fifoReady, fifoOut) = toSignals (axiPayloadFifo fifoDepth) (fifoIn, Axi4StreamS2M <$> (fmap _tready axiOutS2M .&&. consumeFifo))

  goMealy packetComplete (fifoOutGo, Axi4StreamS2M fifoReadyGo, axiM2SGo) =
    (packetCompleteNext, (consumeFifoGo, consumeAxiGo))
   where
    consumeFifoGo = packetComplete || not fifoReadyGo
    consumeAxiGo = not packetComplete && fifoReadyGo
    packetCompleteNext =
      if packetComplete
      then isJust fifoOutGo
      else fifoReadyGo && maybe False _tlast axiM2SGo
