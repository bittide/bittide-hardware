-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Bittide.Axi4 (
  -- * Scaling circuits
  axiStreamFromByteStream,
  axiStreamToByteStream,
  axiPacking,

  -- * Wishbone interfaces
  wbAxisRxBufferCircuit,
  wbToAxi4StreamTx,

  -- * 2-domain FIFOs for each AXI4 channel
  axi4ReadAddressFifo,
  axi4ReadDataFifo,
  axi4WriteAddressFifo,
  axi4WriteDataFifo,
  axi4WriteResponseFifo,

  -- * Other circuits
  axiStreamPacketFifo,
  ilaAxi4Stream,
  rxReadMasterC,

  -- * Utility functions
  combineAxi4Stream,
  splitAxi4Stream,
  packAxi4Stream,
  eqAxi4Stream,
  axiUserMap,
  axiUserMapC,
  isPackedTransfer,

  -- * Internal
  mkKeep,
) where

import Clash.Prelude

import Data.Constraint
import Data.Constraint.Nat.Lemmas
import Data.Maybe
import Data.Proxy

import Bittide.Axi4.Internal
import Bittide.Extra.Maybe
import Bittide.SharedTypes
import Clash.Protocols.Axi4.Extra

import Clash.Cores.Xilinx.DcFifo (dcFifoDf)
import Clash.Cores.Xilinx.Ila hiding (Data)
import Clash.Sized.Internal.BitVector (popCountBV)

import Protocols
import Protocols.Axi4.ReadAddress
import Protocols.Axi4.ReadData
import Protocols.Axi4.Stream as AS
import Protocols.Axi4.WriteAddress
import Protocols.Axi4.WriteData
import Protocols.Axi4.WriteResponse
import Protocols.Wishbone as WB

import qualified Protocols.DfConv as DfConv

{- $setup
>>> import Clash.Prelude
>>> import Protocols.Axi4.Stream
-}

{- | An 'Axi4Stream' without gaps in the data. This means that for each transfer
the following holds:

* For a transfer with _tlast deasserted, all _tkeep bools are set.
* For a transfer with _tlast asserted, the first /n/ bools of _tkeep are set,
  where n is the number of bytes in the transfer.
-}
type PackedAxi4Stream dom conf userType = Axi4Stream dom conf userType

{-# NOINLINE axiStreamFromByteStream #-}

{- | Transforms an 'Axi4Stream' of 1 byte wide into an 'Axi4Stream' of /n/ bytes
wide. If it encounters '_tlast' or has captured /n/ bytes, it will present
the transfer at the output. Note that if less than /n/ bytes have been
captured, but '_tlast' is set, the component will output the captured bytes
with appropriately set '_tkeep' bits. The '_tuser', _tdest' and '_tid' signals
are blindly routed to the output. This effectively means that all but the
last '_tuser', '_tdest', '_tid' are linked to a valid transfer.

TODO: Add test that verifies throughput requirements.
TODO: Make user specify the number of bytes to capture, instead of number of
      bytes minus one (@addedWidth@).
-}
axiStreamFromByteStream ::
  forall dom addedWidth idWidth destWidth userType.
  ( HiddenClockResetEnable dom
  , KnownNat addedWidth
  , KnownNat idWidth
  , KnownNat destWidth
  , Eq userType
  , NFDataX userType
  , Show userType
  ) =>
  Circuit
    (Axi4Stream dom ('Axi4StreamConfig 1 idWidth destWidth) userType)
    ( PackedAxi4Stream
        dom
        ('Axi4StreamConfig (addedWidth + 1) idWidth destWidth)
        (Vec (addedWidth + 1) userType)
    )
axiStreamFromByteStream = AS.forceResetSanity |> Circuit (mealyB go Nothing)
 where
  go axiStored ~(input, Axi4StreamS2M{_tready = outputReady}) =
    (axiNext, (Axi4StreamS2M inputReady, output))
   where
    undefUser = deepErrorX "axiStreamFromByteStream: _tuser undefined"

    -- Try to append the incoming axi to the stored axi.
    dropInput = maybe False (\a -> not (or (_tlast a :> _tkeep a))) input
    combinedAxi = axiUserMap (uncurry (:<)) <$> combineAxi4Stream axiStored input

    -- Shift the internal axi towards HEAD by one position.
    -- If the head of the pre-shifted axi has its keep bit set, shifting is done.
    extendedAxi = fmap (axiUserMap (:< undefUser) . extendAxi @_ @1) axiStored
    axiPreShift = combinedAxi <|> extendedAxi
    axiPostShift =
      snd
        $ splitAxi4Stream @1
        $ fmap (axiUserMap (\v -> (head v, tail v))) axiPreShift

    -- Output the pre-shifted axi if we can not shift anymore.
    shiftingDone = not dropInput && maybe False isPackedTransfer axiPreShift
    capturedLast = maybe False _tlast axiPreShift
    output = if shiftingDone then axiPreShift else Nothing

    -- Flow control
    (axiNext, inputReady)
      | dropInput = (axiStored, True) -- Drop the input
      | shiftingDone && outputReady = (Nothing, isJust combinedAxi) -- valid output, accepted
      | shiftingDone && not outputReady = (axiStored, False) -- valid output, not accepted
      | not shiftingDone && isJust input = (axiPostShift, isJust combinedAxi) -- Shift when input
      | not shiftingDone && capturedLast = (axiPostShift, False) -- Shift when captured _tlast
      | otherwise = (axiStored, False) -- No input

-- TODO: Add test that verifies throughput requirements.

{- | Transforms an Axi4 stream of /n/ bytes wide into an Axi4 stream of 1 byte
wide. It stores the incoming transfer and shifts it out one by one. The incoming
transfer is acknowledged when the last byte is acknowledged by the outgoing transfer.
The '_tuser', '_tdest' and '_tid' are blindly routed to the output.
-}
axiStreamToByteStream ::
  forall dom dataWidth idWidth destWidth userType.
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , KnownNat idWidth
  , KnownNat destWidth
  , Eq userType
  , NFDataX userType
  , Show userType
  ) =>
  Circuit
    (PackedAxi4Stream dom ('Axi4StreamConfig dataWidth idWidth destWidth) userType)
    (Axi4Stream dom ('Axi4StreamConfig 1 idWidth destWidth) userType)
axiStreamToByteStream = AS.forceResetSanity |> Circuit (mealyB go Nothing)
 where
  go axiStored (input, Axi4StreamS2M{_tready = outputReady}) = (axiNext, (inputReady, output))
   where
    (output, axiRest) = splitAxi4Stream @1 (combineAxi4Stream axiStored Nothing)
    axiNext
      | isNothing output || outputReady = axiRest <|> input
      | otherwise = axiStored
    inputReady = Axi4StreamS2M{_tready = isNothing axiRest && (isNothing output || outputReady)}

type EndOfPacket = Bool
type BufferFull = Bool

data WbAxisRxBufferState bufferDepth wbBytes = WbAxisRxBufferState
  { readingBuffer :: Bool
  , packetLength :: Index (bufferDepth * wbBytes + 1)
  , writeCounter :: Index bufferDepth
  , packetComplete :: Bool
  , bufferFull :: Bool
  , abortPacket :: Bool
  }
  deriving (Generic, NFDataX, Show)

{-# NOINLINE wbAxisRxBuffer #-}

-- TODO: Replace with PacketStream

{- | A wishbone accessible buffer of configurable depth that can store a single Axi4Stream packet.
The wishbone interface offers access to the buffer and exposes a status register that indicates:
 * If the buffer contains a packet
 * If the buffer is full before, but does not contain a whole packet.

The wishbone addressing must be 4 byte aligned and is as follows:
 * 0 .. 4 * (fifoDepth - 1) = Read-only access into the buffer.
 * 4 * fifoDepth            = Byte count register.
 * 4 * (fifoDepth + 1)      = Status register

After reading a packet, the byte count must be set to 0 and the status register must be
cleared. The incoming Axi4Stream interface contains a side channel that can be used to abort
the incoming packet. If a packet is aborted, the buffer will consume the remaining transfers
until the end of the packet is reached, after which it will reset the buffer to its initial state.
-}
wbAxisRxBufferCircuit ::
  forall dom wbAddrW wbBytes bufferBytes.
  ( HiddenClockResetEnable dom
  , KnownNat wbAddrW
  , KnownNat wbBytes
  , 1 <= wbBytes
  , 1 <= bufferBytes
  ) =>
  -- | Number of bytes that can be stored in the buffer.
  SNat bufferBytes ->
  Circuit
    ( Wishbone dom 'Standard wbAddrW (Bytes wbBytes)
    , Axi4Stream dom ('Axi4StreamConfig wbBytes 0 0) Bool
    )
    (CSignal dom (EndOfPacket, BufferFull))
wbAxisRxBufferCircuit bytes =
  circuit $ \(wb0, axi0) -> do
    axi1 <- AS.forceResetSanity -< axi0
    wb1 <- WB.forceResetSanity -< wb0
    circ0 -< (wb1, axi1)
 where
  circ0 = case cancelMulDiv @wbBytes @8 of
    Dict -> Circuit $ \((wbM2S, axiM2S), _) -> do
      let (wbS2M, axiS2M, status) = wbAxisRxBuffer bytes wbM2S axiM2S
       in ((wbS2M, axiS2M), status)

wbAxisRxBuffer ::
  forall dom wbAddrW wbBytes bufferBytes.
  ( HiddenClockResetEnable dom
  , KnownNat wbAddrW
  , KnownNat wbBytes
  , 1 <= wbBytes
  , 1 <= bufferBytes
  ) =>
  -- | Minimum number of bytes that can be stored in the buffer, will be rounded up
  -- to the nearest multiple of wbBytes.
  SNat bufferBytes ->
  -- | Wishbone master bus.
  "wbM2S" ::: Signal dom (WishboneM2S wbAddrW wbBytes (Bytes wbBytes)) ->
  -- | Axi4 Stream master bus.
  "axisM2S" ::: Signal dom (Maybe (Axi4StreamM2S ('Axi4StreamConfig wbBytes 0 0) Bool)) ->
  -- |
  -- 1. Wishbone slave bus
  -- 2. Axi4 Stream slave bus
  -- 3. Status
  ""
    ::: ( "wbS2M" ::: Signal dom (WishboneS2M (Bytes wbBytes))
        , "axisS2M" ::: Signal dom Axi4StreamS2M
        , "status" ::: Signal dom (EndOfPacket, BufferFull)
        )
wbAxisRxBuffer SNat = case strictlyPositiveDivRu @bufferBytes @wbBytes of
  Dict -> case leMult @wbBytes @(DivRU bufferBytes wbBytes) of
    Dict -> wbAxisRxBuffer# (SNat @(DivRU bufferBytes wbBytes))

{- | A wishbone accessible buffer of configurable depth that can store a single Axi4Stream packet.
A read transfer from the buffer takes at least two cycles to complete.

The wishbone interface offers access to the buffer and exposes a status register that indicates:
 * If the buffer contains a packet
 * If the buffer is full before, but does not contain a whole packet.

The wishbone addressing must be 4 byte aligned and is as follows:
 0 .. (bufferBytes - 1) = Read-only access into the buffer.
 bufferBytes            = Byte count register.
 (bufferBytes + 4)      = Status register

After reading a packet, the byte count must be set to 0 and the status register must be
cleared. The incoming Axi4Stream interface contains a side channel that can be used to abort
the incoming packet. If a packet is aborted, the buffer will consume the remaining transfers
until the end of the packet is reached, after which it will reset the buffer to its initial state.
-}
wbAxisRxBuffer# ::
  forall dom wbAddrW wbBytes fifoDepth.
  ( HiddenClockResetEnable dom
  , KnownNat wbAddrW
  , KnownNat wbBytes
  , 1 <= wbBytes
  , 1 <= fifoDepth
  , 1 <= wbBytes * fifoDepth
  ) =>
  -- | Depth of the buffer, each entry in the buffer stores `nBytes` bytes.
  SNat fifoDepth ->
  -- | Wishbone master bus.
  "wbM2S" ::: Signal dom (WishboneM2S wbAddrW wbBytes (Bytes wbBytes)) ->
  -- | Axi4 Stream master bus.
  "axisM2S" ::: Signal dom (Maybe (Axi4StreamM2S ('Axi4StreamConfig wbBytes 0 0) Bool)) ->
  -- |
  -- 1. Wishbone slave bus
  -- 2. Axi4 Stream slave bus
  -- 3. Status
  ""
    ::: ( "wbS2M" ::: Signal dom (WishboneS2M (Bytes wbBytes))
        , "axisS2M" ::: Signal dom Axi4StreamS2M
        , "status" ::: Signal dom (EndOfPacket, BufferFull)
        )
wbAxisRxBuffer# fifoDepth@SNat wbM2S axisM2S = (wbS2M, axisS2M, statusReg)
 where
  fifoOut =
    blockRamU
      NoClearOnReset
      fifoDepth
      bramAddr
      bramWrite
  (wbS2M, axisS2M, bramAddr, bramWrite, statusReg) =
    mealyB go initState (wbM2S, axisM2S, fifoOut)
  initState =
    WbAxisRxBufferState
      { readingBuffer = False
      , packetLength = 0
      , writeCounter = 0
      , packetComplete = False
      , bufferFull = False
      , abortPacket = False
      }
  go ::
    WbAxisRxBufferState fifoDepth wbBytes ->
    ( WishboneM2S wbAddrW wbBytes (Bytes wbBytes)
    , Maybe (Axi4StreamM2S ('Axi4StreamConfig wbBytes 0 0) Bool)
    , Bytes wbBytes
    ) ->
    ( WbAxisRxBufferState fifoDepth wbBytes
    , ( WishboneS2M (Bytes wbBytes)
      , Axi4StreamS2M
      , Index fifoDepth
      , Maybe (Index fifoDepth, Bytes wbBytes)
      , (EndOfPacket, BufferFull)
      )
    )
  go
    WbAxisRxBufferState{..}
    ~(WishboneM2S{..}, maybeAxisM2S, wbData) =
      (newState, output)
     where
      masterActive = busCycle && strobe
      packetLengthAddress = maxBound - 1
      statusAddress = maxBound
      internalAddress = (unpack $ resize addr) :: Index (fifoDepth + 2)
      err = masterActive && (addr > resize (pack statusAddress))

      statusBV = pack (packetComplete, bufferFull)
      wbHandshake = masterActive && not err

      -- Since fetching data from the buffer introduces one cycle of latency, we need to
      -- wait for the next cycle to acknowledge the read.
      (readData, nextReadingBuffer, wbAcknowledge) = case (masterActive, internalAddress) of
        (True, (== packetLengthAddress) -> True) -> (resize $ pack packetLength, False, wbHandshake)
        (True, (== statusAddress) -> True) -> (resize statusBV, False, wbHandshake)
        (True, _) -> (wbData, wbHandshake && not readingBuffer, wbHandshake && readingBuffer)
        (False, _) -> (deepErrorX "undefined", False, False)

      axisReady = abortPacket || not (packetComplete || bufferFull)
      axisHandshake = axisReady && isJust maybeAxisM2S

      output =
        ( (emptyWishboneS2M @(Bytes wbBytes)){readData, err, acknowledge = wbAcknowledge}
        , Axi4StreamS2M axisReady
        , unpack . resize $ pack internalAddress
        , maybeAxisM2S >>= orNothing axisHandshake . (writeCounter,) . pack . reverse . _tdata
        , (packetComplete, bufferFull)
        )

      -- Next state
      (nextPacketComplete, nextBufferFull)
        | wbAcknowledge && writeEnable && internalAddress == statusAddress =
            unpack $ resize writeData
        | axisHandshake =
            ( packetComplete || maybe False _tlast maybeAxisM2S
            , bufferFull || writeCounter == maxBound
            )
        | otherwise = unpack $ statusBV

      nextWriteCounter
        | axisHandshake = satSucc SatBound writeCounter
        | packetComplete || bufferFull = 0
        | otherwise = writeCounter

      popCountKeep = leToPlus @1 @wbBytes popCountBV . pack . _tkeep
      bytesInStream = maybe (0 :: Index (wbBytes + 1)) popCountKeep maybeAxisM2S

      nextPacketLength
        | wbAcknowledge && writeEnable && internalAddress == packetLengthAddress =
            unpack $ resize writeData
        | axisHandshake =
            satAdd SatBound packetLength (bitCoerce $ resize bytesInStream)
        | otherwise =
            packetLength

      newState
        | abortPacket && maybe False _tlast maybeAxisM2S = initState
        | otherwise =
            WbAxisRxBufferState
              { readingBuffer = nextReadingBuffer
              , packetLength = nextPacketLength
              , writeCounter = nextWriteCounter
              , packetComplete = nextPacketComplete
              , bufferFull = nextBufferFull
              , abortPacket = abortPacket || maybe False _tuser maybeAxisM2S
              }

data BufferState fifoDepth wbBytes
  = AwaitingData
  | BufferFull
  | PacketComplete (Index (wbBytes * fifoDepth + 1))
  deriving (Generic, NFDataX, Show)

data ReadStateMachine fifoDepth
  = Idle
  | ReadingPacketSize
  | ReadingPacket (Index (fifoDepth + 1))
  | ClearingPacketLength
  | ClearingStatus
  deriving (Generic, NFDataX, Show)

{- | Circuit capable of reading the wishbone interface of @wbAxisRxBuffer@ and
extracting Axi packets. Mostly useful for verification, but can be synthesized.
The internal statemachine continuously reads the satus register of the buffer,
if the buffer is full or a packet is complete, it will:

1. Read the packet length from the buffer.
2. Read the packet from the buffer.
3. Clear the packet length.
4. Clear the status register.
-}
rxReadMasterC ::
  forall dom nBytes addrWidth bufferBytes.
  ( HiddenClockResetEnable dom
  , 1 <= bufferBytes
  , 1 <= nBytes
  , KnownNat addrWidth
  , KnownNat nBytes
  ) =>
  SNat bufferBytes ->
  Circuit
    ()
    ( Wishbone dom 'Standard addrWidth (Bytes nBytes)
    , Axi4Stream dom ('Axi4StreamConfig nBytes 0 0) ()
    )
rxReadMasterC s = case cancelMulDiv @nBytes @8 of
  Dict -> fromSignals $ \(_, bwd) -> ((), rxReadMaster s bwd)

{- | Circuit capable of reading the wishbone interface of @wbAxisRxBuffer@ and
extracting Axi packets. Mostly useful for verification, but can be synthesized.
The internal statemachine continuously reads the satus register of the buffer,
if the buffer is full or a packet is complete, it will:

1. Read the packet length from the buffer.
2. Read the packet from the buffer.
3. Clear the packet length.
4. Clear the status register.
-}
rxReadMaster ::
  forall dom wbBytes addrWidth bufferBytes.
  ( HiddenClockResetEnable dom
  , 1 <= bufferBytes
  , 1 <= wbBytes
  , KnownNat addrWidth
  , KnownNat wbBytes
  ) =>
  SNat bufferBytes ->
  ( Signal dom (WishboneS2M (Bytes wbBytes))
  , Signal dom Axi4StreamS2M
  ) ->
  ( Signal dom (WishboneM2S addrWidth wbBytes (Bytes wbBytes))
  , Signal dom (Maybe (Axi4StreamM2S ('Axi4StreamConfig wbBytes 0 0) ()))
  )
rxReadMaster SNat = case strictlyPositiveDivRu @bufferBytes @wbBytes of
  Dict -> case leMult @wbBytes @(DivRU bufferBytes wbBytes) of
    Dict -> rxReadMaster# (SNat @(DivRU bufferBytes wbBytes))

{- | Circuit capable of reading the wishbone interface of @wbAxisRxBuffer@ and
extracting Axi packets. Mostly useful for verification, but can be synthesized.
The internal statemachine continuously reads the satus register of the buffer,
if the buffer is full or a packet is complete, it will:

1. Read the packet length from the buffer.
2. Read the packet from the buffer.
3. Clear the packet length.
4. Clear the status register.
-}
rxReadMaster# ::
  forall dom wbBytes addrWidth fifoDepth.
  ( HiddenClockResetEnable dom
  , 1 <= fifoDepth
  , 1 <= wbBytes
  , KnownNat addrWidth
  , KnownNat wbBytes
  ) =>
  SNat fifoDepth ->
  ( Signal dom (WishboneS2M (Bytes wbBytes))
  , Signal dom Axi4StreamS2M
  ) ->
  ( Signal dom (WishboneM2S addrWidth wbBytes (Bytes wbBytes))
  , Signal dom (Maybe (Axi4StreamM2S ('Axi4StreamConfig wbBytes 0 0) ()))
  )
rxReadMaster# SNat = mealyB go (AwaitingData @fifoDepth @wbBytes, Idle)
 where
  go
    (bufState, readState :: ReadStateMachine fifoDepth)
    ~(WishboneS2M{..}, Axi4StreamS2M{..}) = (nextState, (wbM2S, axiM2S))
     where
      -- Driving wishbone signals
      (writeEnable, addr) = case readState of
        Idle -> (False, natToNum @(1 + fifoDepth))
        ClearingStatus -> (True, natToNum @(1 + fifoDepth))
        ReadingPacketSize -> (False, natToNum @fifoDepth)
        ClearingPacketLength -> (True, natToNum @fifoDepth)
        ReadingPacket i -> (False, checkedResize (pack i))

      wbM2S = WishboneM2S{..}
      busCycle = True
      strobe = True
      writeData = 0
      busSelect = maxBound
      lock = False
      cycleTypeIdentifier = Classic
      burstTypeExtension = LinearBurst

      -- Driving Axi signals
      (_tdata, _tstrb, _tid, _tdest, _tuser) = (reverse $ bitCoerce readData, repeat True, 0, 0, ())
      (_tkeep, _tlast) = case (bufState, readState) of
        (PacketComplete s, ReadingPacket i) -> (mkKeep remaining, remaining <= natToNum @wbBytes)
         where
          remaining = satSub SatBound s (checkedResize i `shiftL` 2)
        _ -> (repeat True, False)

      axiM2S = case (readState, acknowledge) of
        (ReadingPacket _, True) -> Just Axi4StreamM2S{..}
        _ -> Nothing

      -- Statemachine control
      nextState =
        if not acknowledge
          then (bufState, readState)
          else case (readState, bufState) of
            (Idle, _) -> case (packetComplete, bufferFull) of
              (True, _) -> (AwaitingData, ReadingPacketSize)
              (_, True) -> (BufferFull, ReadingPacket minBound)
              _ -> (AwaitingData, Idle)
             where
              (packetComplete, bufferFull) = unpack $ resize readData
            (ReadingPacketSize, _) -> (PacketComplete packetSize, ReadingPacket 0)
             where
              packetSize = unpack $ checkedResize readData
            (ReadingPacket i, _)
              | _tready && lastBytes bufState nextReadState -> (bufState, ClearingPacketLength)
              | _tready -> (bufState, nextReadState)
              | otherwise -> (bufState, readState)
             where
              nextReadState = ReadingPacket (satSucc SatBound i)
            (ClearingPacketLength, _) -> (bufState, ClearingStatus)
            (ClearingStatus, _) -> (AwaitingData, Idle)

      lastBytes (PacketComplete s) (ReadingPacket i) = s <= (4 * checkedResize i)
      lastBytes BufferFull (ReadingPacket i) = i == maxBound
      lastBytes _ _ = False

{- | Convert a @n@ number of bytes to an @m@ byte enable Vector to be used with Axi4Stream.

>>> mkKeep @8 @4 3
True :> True :> True :> False :> Nil
>>> mkKeep @8 @4 7
True :> True :> True :> True :> Nil
-}
mkKeep ::
  forall maxIndex byteEnables.
  ( KnownNat maxIndex
  , KnownNat byteEnables
  ) =>
  Index maxIndex ->
  Vec byteEnables Bool
mkKeep nBytes
  -- This can be written more neatly if we fix
  -- https://github.com/clash-lang/clash-compiler/issues/2779
  | nBytes < natToNum @byteEnables = fmap (< checkedResize nBytes) indicesI
  | otherwise = repeat True

type AxiStreamBytesOnly nBytes = 'Axi4StreamConfig nBytes 0 0

-- TODO: Replace with PacketStream

{- | Wishbone to Axi4Stream interface, write operations to address 0 write to the Axi4Stream.
The _tkeep bits are set based on the busSelect bits, when writing to address 1, a transfer
is created that contains no data, but has the _tlast bit set.
-}
wbToAxi4StreamTx ::
  forall dom addrW nBytes.
  (KnownNat addrW, KnownNat nBytes) =>
  Circuit
    (Wishbone dom 'Standard addrW (Bytes nBytes))
    (Axi4Stream dom (AxiStreamBytesOnly nBytes) ())
wbToAxi4StreamTx = case cancelMulDiv @nBytes @8 of
  Dict -> Circuit $ unbundle . fmap go . bundle
   where
    go (WishboneM2S{..}, Axi4StreamS2M{..}) =
      (WishboneS2M{readData, err, acknowledge, retry, stall}, axiM2S)
     where
      masterActive = busCycle && strobe
      addrValid = addr <= 1
      err = masterActive && not (addrValid && writeEnable)
      acknowledge = masterActive && not err && _tready
      readData = 0
      retry = False
      stall = False
      (_tkeep, _tlast)
        | lsb addr == 0 = (reverse $ unpack busSelect, False)
        | otherwise = (repeat False, True)

      _tstrb = repeat False
      _tid = 0
      _tdest = 0
      _tuser = ()
      _tdata = reverse $ unpack writeData
      axiM2S :: Maybe (Axi4StreamM2S (AxiStreamBytesOnly nBytes) ())
      axiM2S = orNothing (masterActive && not err) $ Axi4StreamM2S{..}

data AxiPacketFifoState maxPackets = AxiPacketFifoState
  { packetCount :: Index (maxPackets + 1)
  , newPacketSr :: Vec 2 Bool
  , dumpPacket :: Bool
  }
  deriving (Generic, NFDataX, Show)

-- TODO: Replace with PacketStream

{- | A Fifo circuit for Axi4Stream that stores an entire packet before
producing the packet at the output. If the fifo is full, it will start transmitting
the packet at the output.
-}
axiStreamPacketFifo ::
  forall dom nBytes fifoDepth maxPackets userType.
  ( HiddenClockResetEnable dom
  , 2 <= fifoDepth
  , KnownNat nBytes
  , 1 <= maxPackets
  , NFDataX userType
  ) =>
  SNat maxPackets ->
  SNat fifoDepth ->
  Circuit
    (Axi4Stream dom (AxiStreamBytesOnly nBytes) userType)
    (Axi4Stream dom (AxiStreamBytesOnly nBytes) userType)
axiStreamPacketFifo SNat fifoDepth@SNat = AS.forceResetSanity |> Circuit goCircuit
 where
  goCircuit ~(lhsM2S, fmap _tready -> outputReady) = (Axi4StreamS2M <$> inputReady, output)
   where
    -- I/O Combinatorials
    inputReady = consumeAxi .&&. fifoReady
    output = mux produceFifo fifoOut0 (pure Nothing)
    fifoIn = mux consumeAxi lhsM2S (pure Nothing)

    -- Fifo
    axiProxy = Proxy @(Axi4Stream dom (AxiStreamBytesOnly nBytes) userType)
    fifo = DfConv.fifo axiProxy axiProxy fifoDepth
    (fmap _tready -> fifoReady, fifoOut0) =
      toSignals
        fifo
        (fifoIn, Axi4StreamS2M <$> (produceFifo .&&. outputReady))

    -- I/O Control
    initState = AxiPacketFifoState 0 (repeat False) False :: AxiPacketFifoState maxPackets
    (consumeAxi, produceFifo) = mealyB go initState (lhsM2S, fifoOut0, fifoReady, outputReady)

    go s@AxiPacketFifoState{..} (inpM2S, fifoOut1, fifoReady1, outReady) =
      (nextState, (consumeInp, produceOut))
     where
      addPacket = maybe False _tlast inpM2S && consumeInp && fifoReady1
      subPacket = maybe False _tlast fifoOut1 && produceOut && outReady
      packetCount1 = (if subPacket then satPred SatBound else id) packetCount
      packetCount2 = (if head newPacketSr then satSucc SatBound else id) packetCount1

      stallInp = packetCount == maxBound && or newPacketSr
      consumeInp = not stallInp
      dumpPacket1 =
        (not dumpPacket && isJust inpM2S && not fifoReady1)
          || (dumpPacket && maybe False _tlast inpM2S)
      produceOut = dumpPacket || packetCount /= minBound
      newPacketSr1 = newPacketSr <<+ addPacket

      nextState
        | stallInp = s{packetCount = packetCount1}
        | otherwise = AxiPacketFifoState packetCount2 newPacketSr1 dumpPacket1

-- TODO: Add test that verifies throughput requirements.

{- | Circuit to convert a sparse stream into a contiguous stream while remaining the throughput of
the input stream.
-}
axiPacking ::
  forall dom dataWidth idWidth destWidth.
  ( HiddenClockResetEnable dom
  , 1 <= dataWidth
  , KnownNat dataWidth
  , KnownNat idWidth
  , KnownNat destWidth
  ) =>
  Circuit
    (Axi4Stream dom ('Axi4StreamConfig dataWidth idWidth destWidth) ())
    (PackedAxi4Stream dom ('Axi4StreamConfig dataWidth idWidth destWidth) ())
axiPacking = AS.forceResetSanity |> Circuit (mealyB go Nothing)
 where
  go axiStored ~(input, Axi4StreamS2M{_tready = outputReady}) =
    (axiNext, (Axi4StreamS2M inputReady, output))
   where
    -- undefUser = deepErrorX "axiStreamFromByteStream: _tuser undefined"

    -- Try to append the incoming axi to the stored axi.
    dropInput = maybe False (\a -> not (or (_tlast a :> _tkeep a))) input
    combinedAxi = axiUserMap (const ()) <$> combineAxi4Stream axiStored input

    -- Shift the internal axi towards HEAD by one position.
    -- If the head of the pre-shifted axi has its keep bit set, shifting is done.
    extendedAxi = fmap extendAxi axiStored
    packedAxi = fmap packAxi4Stream $ combinedAxi <|> extendedAxi

    (outputBuffer, excessBuffer) = splitAxi4Stream $ fmap (axiUserMap (const ((), ()))) packedAxi

    -- Output the pre-shifted axi if we can not shift anymore.
    shiftingDone = not dropInput && maybe False isPackedTransfer outputBuffer
    capturedLast = maybe False _tlast outputBuffer
    output = if shiftingDone then outputBuffer else Nothing

    -- Flow control
    (axiNext, inputReady)
      | dropInput = (axiStored, True) -- Drop the input
      | shiftingDone && outputReady = (excessBuffer, isJust combinedAxi) -- valid output, accepted
      | shiftingDone && not outputReady = (axiStored, False) -- valid output, not accepted
      | not shiftingDone && isJust input = (outputBuffer, isJust combinedAxi) -- Shift when input
      | not shiftingDone && capturedLast = (outputBuffer, False) -- Shift when captured _tlast
      | otherwise = (axiStored, False) -- No input

-- | Integrated logic analyzer for an Axi4Stream bus, it captures the data, keep, ready and last signals.
ilaAxi4Stream ::
  forall dom conf userType.
  (HiddenClock dom, KnownAxi4StreamConfig conf) =>
  -- | Number of registers to insert at each probe. Supported values: 0-6.
  -- Corresponds to @C_INPUT_PIPE_STAGES@. Default is @0@.
  Index 7 ->
  -- | Number of samples to store. Corresponds to @C_DATA_DEPTH@. Default set
  -- by 'ilaConfig' equals 'D4096'.
  Depth ->
  Circuit
    (Axi4Stream dom conf userType)
    (Axi4Stream dom conf userType)
ilaAxi4Stream stages0 depth0 = Circuit $ \(m2s, s2m) ->
  let
    ilaInst :: Signal dom ()
    ilaInst =
      ila
        ( ilaConfig
            $ "m2s_tdata"
            :> "m2s_tkeep"
            :> "m2s_tlast"
            :> "s2m_tready"
            :> Nil
        )
          { advancedTriggers = True
          , stages = stages0
          , depth = depth0
          }
        hasClock
        (_tdata . fromJust <$> m2s)
        (_tkeep . fromJust <$> m2s)
        (_tlast . fromJust <$> m2s)
        (_tready <$> s2m)
   in
    ilaInst `hwSeqX` (s2m, m2s)

{- | A packed transfer is a transfer where either:
* _tlast is not set and all _tkeep bits are set.
* _tlast is set and only the first n _tkeep bits are set.
>>> let mkAxi keep last = Axi4StreamM2S @('Axi4StreamConfig 2 0 0) (repeat 0) keep (repeat True) last 0 0 ()
>>> isPackedTransfer $ mkAxi (False :> False :> Nil) False
False
>>> isPackedTransfer $ mkAxi (True :> False :> Nil) False
False
>>> isPackedTransfer $ mkAxi (True :> True :> Nil) False
True
>>> isPackedTransfer $ mkAxi (False :> False :> Nil) True
True
>>> isPackedTransfer $ mkAxi (False :> True :> Nil) True
False
-}
isPackedTransfer :: (KnownNat (DataWidth conf)) => Axi4StreamM2S conf a -> Bool
isPackedTransfer Axi4StreamM2S{..}
  | _tlast = not $ hasGaps _tkeep
  | otherwise = and _tkeep
 where
  rising = snd . mapAccumL (\prevKeep keep -> (keep, not prevKeep && keep)) True
  hasGaps = or . rising

-- | A 2-domain Xilinx FIFO for the AXI4 WriteAddress channel
axi4WriteAddressFifo ::
  forall domA domB conf userType.
  ( KnownDomain domA
  , KnownDomain domB
  , KnownAxi4WriteAddressConfig conf
  , NFDataX userType
  ) =>
  Clock domA ->
  Reset domA ->
  Clock domB ->
  Reset domB ->
  Circuit
    (Axi4WriteAddress domA conf userType)
    (Axi4WriteAddress domB conf userType)
axi4WriteAddressFifo clkA rstA clkB rstB =
  axi4ToDf |> dcFifoDf d5 clkA rstA clkB rstB |> dfToAxi4
 where
  axi4ToDf ::
    Circuit
      (Axi4WriteAddress dom conf userType)
      (Df dom (M2S_WriteAddress conf userType))
  axi4ToDf = Circuit toDf
   where
    toDf (waM2S, dfAck) = (waS2M, dfData)
     where
      waS2M = S2M_WriteAddress . fromAck <$> dfAck
      dfData = orNothing <$> fmap isWriteAddress waM2S <*> waM2S
      fromAck (Ack b) = b

  dfToAxi4 ::
    Circuit
      (Df dom (M2S_WriteAddress conf userType))
      (Axi4WriteAddress dom conf userType)
  dfToAxi4 = Circuit toAxi4
   where
    toAxi4 (dfData, waS2M) = (dfAck, waM2S)
     where
      dfAck = Ack . (._awready) <$> waS2M
      waM2S = fromMaybe M2S_NoWriteAddress <$> dfData

-- | A 2-domain Xilinx FIFO for the AXI4 WriteData channel
axi4WriteDataFifo ::
  forall domA domB conf userType.
  ( KnownDomain domA
  , KnownDomain domB
  , KnownAxi4WriteDataConfig conf
  , NFDataX userType
  ) =>
  Clock domA ->
  Reset domA ->
  Clock domB ->
  Reset domB ->
  Circuit
    (Axi4WriteData domA conf userType)
    (Axi4WriteData domB conf userType)
axi4WriteDataFifo clkA rstA clkB rstB =
  axi4ToDf |> dcFifoDf d5 clkA rstA clkB rstB |> dfToAxi4
 where
  axi4ToDf ::
    Circuit
      (Axi4WriteData dom conf userType)
      (Df dom (M2S_WriteData conf userType))
  axi4ToDf = Circuit toDf
   where
    toDf (wdM2S, dfAck) = (wdS2M, dfData)
     where
      wdS2M = S2M_WriteData . fromAck <$> dfAck
      dfData = orNothing <$> fmap isWriteData wdM2S <*> wdM2S
      fromAck (Ack b) = b

  dfToAxi4 ::
    Circuit
      (Df dom (M2S_WriteData conf userType))
      (Axi4WriteData dom conf userType)
  dfToAxi4 = Circuit toAxi4
   where
    toAxi4 (dfData, wdS2M) = (dfAck, wdM2S)
     where
      dfAck = Ack . (._wready) <$> wdS2M
      wdM2S = fromMaybe M2S_NoWriteData <$> dfData

-- | A 2-domain Xilinx FIFO for the AXI4 WriteResponse channel
axi4WriteResponseFifo ::
  forall domA domB conf userType.
  ( KnownDomain domA
  , KnownDomain domB
  , KnownAxi4WriteResponseConfig conf
  , NFDataX userType
  ) =>
  Clock domA ->
  Reset domA ->
  Clock domB ->
  Reset domB ->
  Circuit
    (Axi4WriteResponse domA conf userType)
    (Axi4WriteResponse domB conf userType)
axi4WriteResponseFifo clkA rstA clkB rstB =
  axi4ToDf |> dcFifoDf d5 clkA rstA clkB rstB |> dfToAxi4
 where
  axi4ToDf ::
    Circuit
      (Axi4WriteResponse dom conf userType)
      (Df dom (S2M_WriteResponse conf userType))
  axi4ToDf = Circuit toDf
   where
    toDf (wrS2M, dfAck) = (wrM2S, dfData)
     where
      wrM2S = M2S_WriteResponse . fromAck <$> dfAck
      dfData = orNothing <$> fmap isWriteResponse wrS2M <*> wrS2M
      fromAck (Ack b) = b

  dfToAxi4 ::
    Circuit
      (Df dom (S2M_WriteResponse conf userType))
      (Axi4WriteResponse dom conf userType)
  dfToAxi4 = Circuit toAxi4
   where
    toAxi4 (dfData, wrM2S) = (dfAck, wrS2M)
     where
      dfAck = Ack . (._bready) <$> wrM2S
      wrS2M = fromMaybe S2M_NoWriteResponse <$> dfData

-- | A 2-domain Xilinx FIFO for the AXI4 ReadAddress channel
axi4ReadAddressFifo ::
  forall domA domB conf userType.
  ( KnownDomain domA
  , KnownDomain domB
  , KnownAxi4ReadAddressConfig conf
  , NFDataX userType
  ) =>
  Clock domA ->
  Reset domA ->
  Clock domB ->
  Reset domB ->
  Circuit
    (Axi4ReadAddress domA conf userType)
    (Axi4ReadAddress domB conf userType)
axi4ReadAddressFifo clkA rstA clkB rstB =
  axi4ToDf |> dcFifoDf d5 clkA rstA clkB rstB |> dfToAxi4
 where
  axi4ToDf ::
    Circuit
      (Axi4ReadAddress dom conf userType)
      (Df dom (M2S_ReadAddress conf userType))
  axi4ToDf = Circuit toDf
   where
    toDf (raM2S, dfAck) = (raS2M, dfData)
     where
      raS2M = S2M_ReadAddress . fromAck <$> dfAck
      dfData = orNothing <$> fmap isReadAddress raM2S <*> raM2S
      fromAck (Ack b) = b

  dfToAxi4 ::
    Circuit
      (Df dom (M2S_ReadAddress conf userType))
      (Axi4ReadAddress dom conf userType)
  dfToAxi4 = Circuit toAxi4
   where
    toAxi4 (dfData, raS2M) = (dfAck, raM2S)
     where
      dfAck = Ack . (._arready) <$> raS2M
      raM2S = fromMaybe M2S_NoReadAddress <$> dfData

-- | A 2-domain Xilinx FIFO for the AXI4 ReadData channel
axi4ReadDataFifo ::
  forall domA domB conf userType dataType.
  ( KnownDomain domA
  , KnownDomain domB
  , KnownAxi4ReadDataConfig conf
  , NFDataX userType
  , NFDataX dataType
  ) =>
  Clock domA ->
  Reset domA ->
  Clock domB ->
  Reset domB ->
  Circuit
    (Axi4ReadData domA conf userType dataType)
    (Axi4ReadData domB conf userType dataType)
axi4ReadDataFifo clkA rstA clkB rstB =
  axi4ToDf |> dcFifoDf d5 clkA rstA clkB rstB |> dfToAxi4
 where
  axi4ToDf ::
    Circuit
      (Axi4ReadData dom conf userType dataType)
      (Df dom (S2M_ReadData conf userType dataType))
  axi4ToDf = Circuit toDf
   where
    toDf (rdS2M, dfAck) = (rdM2S, dfData)
     where
      rdM2S = M2S_ReadData . fromAck <$> dfAck
      dfData = orNothing <$> fmap isReadData rdS2M <*> rdS2M
      fromAck (Ack b) = b

  dfToAxi4 ::
    Circuit
      (Df dom (S2M_ReadData conf userType dataType))
      (Axi4ReadData dom conf userType dataType)
  dfToAxi4 = Circuit toAxi4
   where
    toAxi4 (dfData, rdM2S) = (dfAck, rdS2M)
     where
      dfAck = Ack . (._rready) <$> rdM2S
      rdS2M = fromMaybe S2M_NoReadData <$> dfData
