-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Bittide.Axi4 where

import Clash.Prelude

import Clash.Sized.Internal.BitVector (popCountBV)
import Data.Maybe
import Protocols
import Protocols.Axi4.Stream
import Protocols.Wishbone

import Bittide.Extra.Maybe
import Bittide.SharedTypes

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
      | otherwise                              = (wbData, wbHandshake && not readingFifo, wbHandshake && readingFifo)

    axisReady = not (packetComplete || bufferFull)
    axisHandshake = axisReady && isJust maybeAxisM2S

    output =
      ( (emptyWishboneS2M @(Bytes nBytes)){readData, err, acknowledge = wbAcknowledge}
      , Axi4StreamS2M axisReady
      , resize internalAddress
      , if axisHandshake then (writeCounter,) . pack . _tdata <$> maybeAxisM2S else Nothing
      , (packetComplete, bufferFull)
      )

    -- Next state
    (nextPacketComplete, nextBufferFull)
      | wbAcknowledge && writeEnable && internalAddress == statusAddress
        = unpack $ (\ a b -> a `xor` (a .&. b)) statusBV (resize writeData)
      | axisHandshake = (packetComplete || maybe False _tlast maybeAxisM2S, bufferFull || writeCounter == maxBound)
      | otherwise = unpack $ statusBV `xor` pack clearStatus

    nextWriteCounter
      | axisHandshake  = satSucc SatBound writeCounter
      | packetComplete || bufferFull = 0
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

{-# NOINLINE wbAxisTxBuffer #-}
-- | A wishbone accessible buffer that can transmit Axi4 stream packets.
-- Axi4 Stream packets can be written to the buffer over wishbone and the transmission
-- can be initiated by writing the size of the packet in words to a specific address.
--
-- The wishbone addressing must be 4 byte aligned and is as follows:
--  * 0 .. 4 * (fifoDepth - 1) = Read-only access into the buffer.
--  * 4 * fifoDepth            = Initiate transmission.
wbAxisTxBuffer ::
  forall dom addrW nBytes fifoDepth conf .
  ( HiddenClockResetEnable dom
  , 1 <= fifoDepth
  , DataWidth conf ~ nBytes
  , 2 <= addrW
  , KnownNat addrW
  , KnownAxi4StreamConfig conf) =>
  -- | Depth of the buffer.
  SNat fifoDepth ->
  -- | Wishbone master bus.
  Signal dom (WishboneM2S addrW nBytes (Bytes nBytes)) ->
  -- | Axi4 stream slave bus.
  Signal dom Axi4StreamS2M ->
  -- |
  -- 1. Wishbone slave bus
  -- 2. Axi4 Stream master bus
  ( Signal dom (WishboneS2M (Bytes nBytes))
  , Signal dom (Maybe (Axi4StreamM2S conf Bool)))
wbAxisTxBuffer SNat wbM2S axisS2M = (wbS2M, axisM2S)
 where
  (wbS2M, axisM2S, bramWrite, readAddr) = mealyB go initState (wbM2S, axisS2M, bramOut)
  initState = (0,0,False)
  bramOut =
    blockRamU NoClearOnReset (SNat @fifoDepth)
    (const $ errorX "wbAxisRxBuffer: reset function undefined")
    readAddr bramWrite
  go (readCounter, wordCount, axisValid)
    (WishboneM2S{..}, Axi4StreamS2M{..}, (bramKeeps,bramData)) = (newState, output)
   where
    masterActive = busCycle && strobe
    (alignedAddress, alignment) = split @_ @(addrW - 2) @2 addr
    wordCounterAddress = maxBound :: Index (fifoDepth + 1)
    err = masterActive && (alignment /= 0 || alignedAddress > resize (pack wordCounterAddress))
    acknowledge = masterActive && not err && not (axisValid && writeEnable)
    validWrite = acknowledge && writeEnable && (not axisValid || internalAddress < resize readCounter)
    internalAddress = (unpack $ resize alignedAddress) :: Index (fifoDepth + 1)

    newState = (readCounterNext, wordCountNext, axisValidNext)
    newWordCount = validWrite && internalAddress == wordCounterAddress
    lastTransmit = axisHandshake && readCounter == wordCount

    readCounterNext
      | lastTransmit  = 0
      | axisHandshake = satSucc SatBound readCounter
      | otherwise     = readCounter

    wordCountNext
      | newWordCount = unpack $ resize writeData
      | lastTransmit = 0
      | otherwise    = wordCount

    writeOp
      | validWrite && internalAddress < maxBound
      = Just (resize @_ @_ @fifoDepth internalAddress, (busSelect, writeData))
      | otherwise = Nothing

    axisValidNext
      | newWordCount = True
      | lastTransmit = False
      | otherwise    = axisValid

    axisHandshake = axisValid && _tready
    readData = resize $ pack wordCount
    retry = False
    stall = False

    _tdata = reverse $ unpack bramData
    _tkeep = reverse $ unpack bramKeeps
    _tstrb = repeat True
    _tlast = readCounter == wordCount
    _tid = 0
    _tdest = 0
    _tuser = False
    output =
      ( WishboneS2M{acknowledge, err, readData,retry, stall}
      , if axisValid then Just Axi4StreamM2S{_tdata, _tkeep, _tstrb, _tlast, _tid, _tdest, _tuser} else Nothing
      , writeOp
      , if axisHandshake then readCounterNext else readCounter
      )
