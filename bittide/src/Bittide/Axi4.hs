-- SPDX-FileCopyrightText: 2023-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MagicHash #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Bittide.Axi4
(
  -- Scaling circuits
  axisFromByteStream,
  axisToByteStream,
  axiPacking,

  -- Wishbone interfaces
  wbAxisRxBufferCircuit,
  wbAxisRxBuffer,
  wbToAxiTx,

  -- Other circuits
  axiStreamPacketFifo,
  ilaAxi4Stream,
  rxReadMasterC,

  -- Utility functions
  combineAxi4Stream,
  splitAxi4Stream,
  packAxi4Stream,
  dropUserAxi4Stream,
  eqAxi4Stream,
) where

import Clash.Prelude

import Clash.Cores.Xilinx.Ila
import Clash.Sized.Internal.BitVector (popCountBV)
import Data.Constraint
import Data.Maybe
import Data.Proxy
import Protocols
import Protocols.Axi4.Stream
import Protocols.Wishbone
import Clash.Debug

import Debug.Trace
import Bittide.Extra.Maybe
import Bittide.SharedTypes
import Data.Constraint.Nat.Extra
import qualified Protocols.DfConv as DfConv

{- $setup
>>> import Clash.Prelude
-}

-- | An Axi4Stream in with no gaps in the data. For each transaction the following holds:
--
-- * For a transaction that is not the last in a packet, all _tkeep bits are set.
-- * For a transaction that is the last in a packet, the _tlast bit is set and the
--   first n bits of _tkeep are set, where n is the number of bytes in the transaction.
type PackedAxi4Stream dom conf userType = Axi4Stream dom conf userType

deriving instance (KnownAxi4StreamConfig conf, BitPack userType) => BitPack (Axi4StreamM2S conf userType)
deriving instance BitPack Axi4StreamS2M

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
  forall dom addedWidth idWidth destWidth userType .
  ( HiddenClockResetEnable dom
  , KnownNat addedWidth
  , KnownNat idWidth
  , KnownNat destWidth
  , Eq userType
  , NFDataX userType) =>
  Circuit
    (Axi4Stream dom ('Axi4StreamConfig 1 idWidth destWidth) userType)
    (PackedAxi4Stream dom
      ('Axi4StreamConfig (addedWidth + 1) idWidth destWidth)
      (Vec (addedWidth + 1) userType))
axisFromByteStream = Circuit (mealyB go Nothing)
 where
  go axiStored (input, Axi4StreamS2M{_tready = outputReady}) = (axiNext, (inputReady, output))
   where
    undefUser = deepErrorX "axisFromByteStream: _tuser undefined"
    -- Try to append the incoming axi to the stored axi
    combinedAxi = axiUserMap (\(v1, v2) -> v1 :< v2) <$> combineAxi4Stream axiStored input
    inpPass = isJust input && isJust combinedAxi
    inpBlocked = isJust input && isNothing combinedAxi

    -- Shifting bytes
    axiPreShift = combinedAxi <|> (axiUserMap extendWithErrorX . extendAxi <$> axiStored)
    axiBytes =
      zipWith4 (\d k s u -> orNothing k (d, s, u))
      (maybe (repeat 0) _tdata axiPreShift)
      (maybe (repeat False) _tkeep axiPreShift)
      (maybe (repeat False) _tstrb axiPreShift)
      (maybe (repeat undefUser) _tuser axiPreShift)

    -- TODO: Replace shiftPackVec with a more resource efficient implementation.
    -- https://github.com/bittide/bittide-hardware/issues/521
    (newData, newKeeps, newStrobes, newUsers) = unzip4 $
      (maybe (0, False, False, undefUser) (\(d, s, u) -> (d, True, s, u))) <$> shiftPackVec axiBytes

    axiPostShift =
      (\a -> a{ _tdata = newData, _tkeep = newKeeps, _tstrb = newStrobes, _tuser = newUsers})
      <$> axiPreShift

    shiftingDone = fromMaybe False
      $ (\pre post -> _tkeep pre == _tkeep post) <$> axiPreShift <*> axiPostShift

    -- Axi RHS
    -- The rhs axi is valid if we can not shift in more bytes and one of the following holds:
    -- * All keep bits of the output are set
    -- * We can not combine the lhs axi with the stored axi
    -- * The last bit of the output is set.
    outputValid = shiftingDone && (and newKeeps || inpBlocked || maybe False _tlast axiPostShift)
    output
      | outputValid = axiPostShift
      | otherwise = Nothing
    outputHandshake = outputValid && outputReady

    -- Axi LHS
    -- The lhs is acknowledged if we can combine it with the stored axi and one
    -- of the following holds:
    -- * The rhs is not valid yet
    -- * THe rhs is valid and ready
    inputReady = Axi4StreamS2M $ inpPass && (outputHandshake || not outputValid)

    -- State update
    axiNext
      | outputHandshake = Nothing
      | otherwise    = fst $ splitAxi4Stream @_ @1 $ (axiUserMap (\v -> (init v, ()))) <$> axiPostShift

-- | Shifts all @Just a@ in a `Vec n (Maybe a)` in the direction of head by one position.
-- The last element is replaced by @Nothing@.
-- >>> shiftPackVec @6 @(Unsigned 8) (Just 1 :> Just 2 :> Nothing :> Nothing :> Just 3 :> Nothing :> Nil)
-- Just 1 :> Just 2 :> Nothing :> Just 3 :> Nothing :> Nothing :> Nil
shiftPackVec :: forall n a . (KnownNat n) => Vec (n + 1) (Maybe a) -> Vec (n + 1) (Maybe a)
shiftPackVec v = zipWith3 f canShift v (v <<+ Nothing)
 where
  f getNext current next = if getNext then next else current
  availables = fmap isNothing v
  canShift = scanl (||) (head availables) (tail availables)

-- | Transforms an Axi4 stream of /n/ bytes wide into an Axi4 stream of 1 byte
-- wide. It stores the incoming transaction and shifts it out one by one. The incoming
-- transaction is acknowledged when the last byte is acknowledged by the outgoing transaction.
-- The '_tuser', '_tdest' and '_tid' are blindly routed to the output.
axisToByteStream ::
  forall dom dataWidth idWidth destWidth userType .
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , KnownNat idWidth
  , KnownNat destWidth
  , Eq userType
  , NFDataX userType
  , Show userType) =>
  Circuit
    (PackedAxi4Stream dom ('Axi4StreamConfig dataWidth idWidth destWidth) userType)
    (Axi4Stream dom ('Axi4StreamConfig 1 idWidth destWidth) userType)
axisToByteStream = Circuit (mealyB go Nothing)
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
  } deriving (Generic, NFDataX, Show)

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
-- cleared. The incoming Axi4Stream interface contains a side channel that can be used to abort
-- the incoming packet. If a packet is aborted, the buffer will consume the remaining transactions
-- until the end of the packet is reached, after which it will reset the buffer to its initial state.
wbAxisRxBufferCircuit ::
  forall dom wbAddrW wbBytes bufferBytes .
  ( HiddenClockResetEnable dom
  , KnownNat wbAddrW, 2 <= wbAddrW
  , KnownNat wbBytes, 1 <= wbBytes
  , 1 <= bufferBytes) =>
  -- | Number of bytes that can be stored in the buffer.
  SNat bufferBytes ->
  Circuit
    (Wishbone dom 'Standard wbAddrW (Bytes wbBytes), Axi4Stream dom ('Axi4StreamConfig wbBytes 0 0) Bool)
    (CSignal dom (EndOfPacket, BufferFull))
wbAxisRxBufferCircuit bytes = case cancelMulDiv @wbBytes @8 of
  Dict -> Circuit $ \((wbM2S, axiM2S),_) -> do
    let (wbS2M, axiS2M,status) = wbAxisRxBuffer bytes wbM2S axiM2S
      in ((wbS2M, axiS2M), status)

wbAxisRxBuffer ::
  forall dom wbAddrW wbBytes bufferBytes .
  ( HiddenClockResetEnable dom
  , KnownNat wbAddrW, 2 <= wbAddrW
  , KnownNat wbBytes, 1 <= wbBytes
  , 1 <= bufferBytes) =>
  -- | Minimum number of bytes that can be stored in the buffer, will be rounded up
  -- to the nearest multiple of wbBytes.
  SNat bufferBytes ->
  -- | Wishbone master bus.
  "wbM2S" ::: Signal dom (WishboneM2S wbAddrW wbBytes (Bytes wbBytes)) ->
  -- | Axi4 Stream master bus.
  "axisM2S" ::: Signal dom (Maybe (Axi4StreamM2S ('Axi4StreamConfig wbBytes 0 0) Bool)) ->
  -- | External controls to clear bits in the status register.
  "clearinterrupts" ::: Signal dom (EndOfPacket, BufferFull) ->
  -- |
  -- 1. Wishbone slave bus
  -- 2. Axi4 Stream slave bus
  -- 3. Status
  "" :::
  ( "wbS2M" ::: Signal dom (WishboneS2M (Bytes wbBytes))
  , "axisS2M" ::: Signal dom Axi4StreamS2M
  , "status" ::: Signal dom (EndOfPacket, BufferFull)
  )
wbAxisRxBuffer SNat = case strictlyPositiveDivRu @bufferBytes @wbBytes of
  Dict -> case leMult @wbBytes @(DivRU bufferBytes wbBytes) of
    Dict -> wbAxisRxBuffer# (SNat @(wbBytes * (DivRU bufferBytes wbBytes)))

-- | A wishbone accessible buffer of configurable depth that can store a single Axi4Stream packet.
-- A read transaction from the buffer takes at least two cycles to complete.
--
-- The wishbone interface offers access to the buffer and exposes a status register that indicates:
--  * If the buffer contains a packet
--  * If the buffer is full before, but does not contain a whole packet.
--
-- The wishbone addressing must be 4 byte aligned and is as follows:
--  0 .. (bufferBytes - 1) = Read-only access into the buffer.
--  bufferBytes            = Byte count register.
--  (bufferBytes + 4)      = Status register
--
-- After reading a packet, the byte count must be set to 0 and the status register must be
-- cleared. The incoming Axi4Stream interface contains a side channel that can be used to abort
-- the incoming packet. If a packet is aborted, the buffer will consume the remaining transactions
-- until the end of the packet is reached, after which it will reset the buffer to its initial state.
wbAxisRxBuffer# ::
  forall dom wbAddrW wbBytes fifoDepth  .
  ( HiddenClockResetEnable dom
  , KnownNat wbAddrW, 2 <= wbAddrW
  , KnownNat wbBytes, 1 <= wbBytes
  , 1 <= fifoDepth
  , 1 <= wbBytes * fifoDepth) =>
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
  "" :::
  ( "wbS2M" ::: Signal dom (WishboneS2M (Bytes wbBytes))
  , "axisS2M" ::: Signal dom Axi4StreamS2M
  , "status" ::: Signal dom (EndOfPacket, BufferFull)
  )
wbAxisRxBuffer# fifoDepth@SNat wbM2S axisM2S = (wbS2M, axisS2M, statusReg)
   where
    fifoOut =
      blockRamU NoClearOnReset fifoDepth
      (const $ errorX "wbAxisRxBuffer: reset function undefined")
      bramAddr bramWrite
    (wbS2M, axisS2M, bramAddr, bramWrite, statusReg) =
      mealyB go initState (wbM2S, axisM2S, fifoOut)
    initState = WbAxisRxBufferState
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
        , Axi4StreamS2M, Index fifoDepth
        , Maybe (Index fifoDepth, Bytes wbBytes)
        , (EndOfPacket, BufferFull)
        )
      )
    go
      s@WbAxisRxBufferState{..}
      ~(WishboneM2S{..}, maybeAxisM2S, wbData)
      = (traceShowId $ newState, output)
     where
      masterActive = busCycle && strobe
      (alignedAddress, alignment) = split @_ @(wbAddrW - 2) @2 addr

      packetLengthAddress = maxBound - 1
      statusAddress       = maxBound
      internalAddress     = (bitCoerce $ resize alignedAddress) :: Index (fifoDepth + 2)

      err = masterActive && (alignment /= 0 || alignedAddress > resize (pack statusAddress))

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
        | wbAcknowledge && writeEnable && internalAddress == statusAddress
          = unpack $ resize writeData
        | axisHandshake =
          ( packetComplete || maybe False _tlast maybeAxisM2S
          , bufferFull || writeCounter == maxBound
          )
        | otherwise = unpack $ statusBV

      nextWriteCounter
        | axisHandshake                 = satSucc SatBound writeCounter
        | packetComplete || bufferFull  = 0
        | otherwise                     = writeCounter

      popCountKeep = leToPlus @1 @wbBytes popCountBV . pack ._tkeep
      bytesInStream = maybe (0 :: Index (wbBytes + 1)) popCountKeep maybeAxisM2S

      nextPacketLength
        | wbAcknowledge && writeEnable && internalAddress == packetLengthAddress
          = unpack $ resize writeData
        | axisHandshake
          = satAdd SatBound packetLength (bitCoerce $ resize bytesInStream)
        | otherwise
          = packetLength

      newState
        | abortPacket && maybe False _tlast maybeAxisM2S = initState
        | otherwise = WbAxisRxBufferState
          { readingBuffer = nextReadingBuffer
          , packetLength = nextPacketLength
          , writeCounter = nextWriteCounter
          , packetComplete = nextPacketComplete
          , bufferFull = nextBufferFull
          , abortPacket = abortPacket || maybe False _tuser maybeAxisM2S
          }

data BufferState bufferSize = AwaitingData | BufferFull | PacketComplete (Index (bufferSize + 1))
  deriving (Generic, NFDataX, Show)

data ReadStateMachine bufferSize =
    Idle | ReadingPacketSize | ReadingPacket (Index (bufferSize + 1)) | ClearingPacketLength | ClearingStatus
  deriving (Generic, NFDataX, Show)

-- | Circuit capable of reading the wishbone interface of @wbAxisRxBuffer@ and
-- extracting Axi packets. Mostly useful for verification, but can be synthesized.
-- The internal statemachine continuously reads the satus register of the buffer,
-- if the buffer is full or a packet is complete, it will:
--
-- 1. Read the packet length from the buffer.
-- 2. Read the packet from the buffer.
-- 3. Clear the packet length.
-- 4. Clear the status register.
rxReadMasterC ::
  forall dom nBytes addrWidth bufferBytes .
  ( HiddenClockResetEnable dom
  , 1 <= bufferBytes
  , 1 <= nBytes
  , KnownNat addrWidth
  , KnownNat nBytes) =>
  SNat bufferBytes ->
  Circuit
  ()
  (Wishbone dom 'Standard addrWidth (Bytes nBytes), Axi4Stream dom ('Axi4StreamConfig nBytes 0 0) ())
rxReadMasterC s = case cancelMulDiv @nBytes @8 of
  Dict -> fromSignals $ \ (_, bwd) -> ((), rxReadMaster s bwd)

rxReadMaster ::
  forall dom wbBytes addrWidth bufferBytes .
  ( HiddenClockResetEnable dom
  , 1 <= bufferBytes
  , 1 <= wbBytes
  , KnownNat addrWidth
  , KnownNat wbBytes) =>
  SNat bufferBytes ->
  ( Signal dom (WishboneS2M (Bytes wbBytes))
  , Signal dom Axi4StreamS2M
  ) ->
  ( Signal dom (WishboneM2S addrWidth wbBytes (Bytes wbBytes))
  , Signal dom (Maybe (Axi4StreamM2S ('Axi4StreamConfig wbBytes 0 0) ()))
  )
rxReadMaster SNat = case strictlyPositiveDivRu @bufferBytes @wbBytes of
  Dict -> rxReadMaster# (SNat @(Max 1 (wbBytes * DivRU bufferBytes wbBytes)))

-- | Circuit capable of reading the wishbone interface of @wbAxisRxBuffer@ and
-- extracting Axi packets. Mostly useful for verification, but can be synthesized.
-- The internal statemachine continuously reads the satus register of the buffer,
-- if the buffer is full or a packet is complete, it will:
--
-- 1. Read the packet length from the buffer.
-- 2. Read the packet from the buffer.
-- 3. Clear the packet length.
-- 4. Clear the status register.
rxReadMaster# ::
  forall dom nBytes addrWidth bufferDepth .
  ( HiddenClockResetEnable dom
  , 1 <= bufferDepth * nBytes
  , 1 <= bufferDepth
  , KnownNat addrWidth
  , KnownNat nBytes) =>
  SNat bufferDepth ->
  ( Signal dom (WishboneS2M (Bytes nBytes))
  , Signal dom Axi4StreamS2M
  ) ->
  ( Signal dom (WishboneM2S addrWidth nBytes (Bytes nBytes))
  , Signal dom (Maybe (Axi4StreamM2S ('Axi4StreamConfig nBytes 0 0) ()))
  )
rxReadMaster# SNat = mealyB go (AwaitingData @bufferDepth, Idle)
   where
    go
      (state@(bufState, readState :: ReadStateMachine bufferDepth))
      ~(WishboneS2M{..}, Axi4StreamS2M{..}) = (nextState, (wbM2S, axiM2S))
     where
      -- Driving wishbone signals
      (writeEnable, addr) = case readState of
        Idle                 -> (False, natToNum @(4 * (1 + bufferDepth)))
        ClearingStatus       -> (True, natToNum @(4 * (1 + bufferDepth)))
        ReadingPacketSize    -> (False, natToNum @(4 * bufferDepth))
        ClearingPacketLength -> (True, natToNum @(4 * bufferDepth))
        ReadingPacket i      -> (False, checkedResize (pack i))

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
        (PacketComplete s, ReadingPacket i) -> (mkKeep remaining, remaining <= natToNum @nBytes)
         where
          remaining = satSub SatBound s i
        _ -> (repeat True, False)

      axiM2S = case (readState, acknowledge) of
        (ReadingPacket _, True) -> Just Axi4StreamM2S {..}
        _                       -> Nothing

      -- Statemachine control
      nextState = if not acknowledge then (bufState, readState) else
        case (readState, bufState) of
          (Idle,_) -> case (packetComplete, bufferFull) of
            (True, _) -> (AwaitingData, ReadingPacketSize)
            (_, True) -> (BufferFull, ReadingPacket minBound)
            _   -> (AwaitingData, Idle)
           where
            (packetComplete, bufferFull) = unpack $ resize readData
          (ReadingPacketSize,_) -> (PacketComplete packetSize, ReadingPacket 0)
             where
              packetSize = unpack $ checkedResize readData
          (ReadingPacket i, _)
            | _tready && lastBytes bufState nextReadState -> (bufState, ClearingPacketLength)
            | _tready -> (bufState, nextReadState)
            | otherwise -> (bufState, readState)
             where
              nextReadState = ReadingPacket (satAdd SatBound i (natToNum @nBytes))
          (ClearingPacketLength, _) -> (bufState, ClearingStatus)
          (ClearingStatus,_) -> (AwaitingData, Idle)

      lastBytes (PacketComplete s) (ReadingPacket i) = s <= i
      lastBytes (BufferFull) (ReadingPacket i) = i == maxBound
      lastBytes _ _ = False

-- | Convert a @n@ number of bytes to an @m@ byte enable Vector to be used with Axi4Stream.
mkKeep ::
  forall maxIndex byteEnables .
  ( KnownNat maxIndex
  , KnownNat byteEnables
  ) =>
  Index maxIndex ->
  Vec byteEnables Bool
mkKeep nBytes
  | nBytes < natToNum @byteEnables = fmap (< checkedResize nBytes) indicesI
  | otherwise = repeat True


type AxiStreamBytesOnly nBytes = 'Axi4StreamConfig nBytes 0 0

-- | Wishbone to Axi4Stream interface, write operations to address 0 write to the Axi4Stream.
-- The _tkeep bits are set based on the busSelect bits, when writing to address 1, a transaction
-- is created that contains no data, but has the _tlast bit set.
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
      readData = 0
      retry = False
      stall = False
      (_tkeep, _tlast)
        | lsb internalAddress == 0 = (reverse $ unpack busSelect, False)
        | otherwise                = (repeat False, True)

      _tstrb = repeat False
      _tid = 0
      _tdest = 0
      _tuser = ()
      _tdata = reverse $ unpack writeData
      axiM2S :: Maybe (Axi4StreamM2S (AxiStreamBytesOnly nBytes) ())
      axiM2S = orNothing (masterActive && not err) $ Axi4StreamM2S{..}

data AxiPacketFifoState = AxiAccumulating | AxiPacketComplete Bool | AxiPassThrough
  deriving (Generic, NFDataX, Show)

-- | A Fifo circuit for Axi4Stream that stores an entire packet before
-- producing the packet at the output. If the fifo is full, it will start transmitting
-- the packet at the output.
axiStreamPacketFifo ::
  forall dom nBytes fifoDepth userType .
  ( HiddenClockResetEnable dom
  , KnownNat fifoDepth, 2 <= fifoDepth
  , KnownNat nBytes
  , NFDataX userType) =>
  SNat fifoDepth ->
  Circuit
    (Axi4Stream dom (AxiStreamBytesOnly nBytes) userType)
    (Axi4Stream dom (AxiStreamBytesOnly nBytes) userType)
axiStreamPacketFifo fifoDepth = Circuit goCircuit
 where
  goCircuit (lhsM2S, fmap _tready -> rhsS2M) = (inputReady, output)
   where
    inputReady = Axi4StreamS2M <$> (consumeAxi .&&. fifoReady)
    output = mux produceFifo fifoOut (pure Nothing)
    fifoIn = mux consumeAxi lhsM2S (pure Nothing)
    axiProxy = Proxy @(Axi4Stream dom (AxiStreamBytesOnly nBytes) userType)
    fifo = DfConv.fifo axiProxy axiProxy fifoDepth
    (consumeAxi, produceFifo) = mooreB goState goOut AxiAccumulating (lhsM2S, fifoOut, fifoReady, rhsS2M)
    (fmap _tready -> fifoReady, fifoOut) = toSignals fifo
      (fifoIn, Axi4StreamS2M <$> (rhsS2M .&&. produceFifo))

  goState AxiAccumulating (lhsM2S, fifoOut, fifoReady, _)
    | fifoReady && maybe False _tlast lhsM2S = (AxiPacketComplete False)
    | not fifoReady && isJust fifoOut = AxiPassThrough
    | otherwise = AxiAccumulating
  goState (AxiPacketComplete headDetected) (_, fifoOut, _, rhsS2M)
    | rhsS2M && maybe False _tlast fifoOut = AxiAccumulating
    | headDetected && isNothing fifoOut = AxiAccumulating
    | otherwise = (AxiPacketComplete (headDetected || isJust fifoOut))
  goState AxiPassThrough (lhsM2S, _, fifoReady, _)
    | fifoReady && maybe False _tlast lhsM2S = (AxiPacketComplete True)
    | otherwise = AxiPassThrough

  goOut = \case
    AxiAccumulating -> (True, False)
    (AxiPacketComplete _) -> (True, True)
    AxiPassThrough -> (True, True)

-- | Circuit to convert a sparse stream into a contiguous stream while remaining the throughput of
-- the input stream.
axiPacking ::
  forall dom dataWidth idWidth destWidth .
  ( HiddenClockResetEnable dom
  , 1 <= dataWidth, KnownNat dataWidth, KnownNat idWidth, KnownNat destWidth) =>
  Circuit
    (Axi4Stream dom ('Axi4StreamConfig dataWidth idWidth destWidth) ())
    (PackedAxi4Stream dom ('Axi4StreamConfig dataWidth idWidth destWidth) ())
axiPacking = Circuit (mealyB go (Nothing, False))
 where
  go (_, False) _ = ((Nothing, True), (Axi4StreamS2M False, Nothing))
  go (axiStored, True) ~(input, Axi4StreamS2M outputReady) =
    ((newStored, True), (Axi4StreamS2M consumableInput, output))
   where

    -- Axi Right
    -- Produce when:
    --  Data has overflowed into the excess buffer.
    --  Output buffer contains the end of a packet.
    outputValid = isJust excessBuffer || maybe False _tlast outputBuffer
    output = if outputValid then outputBuffer else Nothing
    handshakeOutput = outputValid && outputReady

    -- Axi Left
    -- Accept input if we can consume the input into the remainder and we don't have valid output being blocked.
    consumableInput = isJust packedAxi4Stream
    inputReady = consumableInput && not (outputValid && not outputReady)
    inputBlock = isJust input && not inputReady

    -- State update
    -- Split the state into the output buffer and the excess buffer.
    -- Determine the remainder and create a new packed Axi4Stream using the remainder and the input.
    (outputBuffer, excessBuffer) = splitAxi4Stream axiStored
    remainder = if handshakeOutput then excessBuffer else outputBuffer
    packedAxi4Stream = packAxi4Stream <$> combineAxi4Stream remainder input

    newStored
      -- The output buffer is consumed and we can add the input to the excess buffer
      | handshakeOutput && consumableInput = packedAxi4Stream
      -- The output buffer is consumed, but We can not add the input to the excess buffer
      | handshakeOutput = combineAxi4Stream excessBuffer Nothing
      -- Our output has not been consumed
      | outputValid = axiStored
      -- We can not consume the input
      | inputBlock = axiStored
      -- We can consume the input
      | otherwise = packedAxi4Stream

-- | Function to move all keep, data and strobes in an Axi4Stream to the front of the vectors based on the
-- _tkeep field.
packAxi4Stream ::
  (KnownAxi4StreamConfig conf) =>
  Axi4StreamM2S conf userType -> Axi4StreamM2S conf userType
packAxi4Stream axi = output
 where
  output = axi{_tdata = newData, _tstrb = newStrobe, _tkeep = newKeep}
  (newData, newKeep, newStrobe) = unzip3
    $ fmap (\ b -> (maybe 0 fst b, isJust b, maybe False snd b)) (packVec inpVec)
  inpVec = orNothing <$> _tkeep axi <*> zip (_tdata axi) (_tstrb axi)

-- | Function that moves all @Just@ values in a `Vec n (Maybe a)` to the front of
-- the vector.
packVec :: KnownNat n => Vec n (Maybe a) -> Vec n (Maybe a)
packVec = foldr f (repeat Nothing)
 where
  f (Just a) acc = Just a +>> acc
  f Nothing acc = acc

-- | Splits an Axi4StreamM2S into a tuple of two Axi4StreamM2S. The first contains
-- all lower bytes of the transaction, the second contains the upper bytes. The first
-- output contains a transaction if at least one of the corresponding keep bits is
-- high, or none of the keep bits are high. The second output will contain a transaction
-- only if at least one of the corresponding keep bits is high. A transaction with
-- only null bytes and _tlast set will produce a transaction with _tlast set in the
-- first output, the second output will be @Nothing@.
splitAxi4Stream ::
  forall widthA widthB idWith destWidth userTypeA userTypeB .
  ( KnownNat widthA
  , KnownNat widthB
  ) =>
  -- | Axi4Stream transaction to split into two transactions.
  Maybe (Axi4StreamM2S ('Axi4StreamConfig (widthA + widthB) idWith destWidth) (userTypeA, userTypeB)) ->
  -- |
  -- 1. Axi4Stream transaction with the first half of the data, keep and strobe vectors.
  -- 2. Axi4Stream transaction with the second half of the data, keep and strobe vectors.
  ( Maybe (Axi4StreamM2S ('Axi4StreamConfig widthA idWith destWidth) userTypeA)
  , Maybe (Axi4StreamM2S ('Axi4StreamConfig widthB idWith destWidth) userTypeB))
splitAxi4Stream Nothing = (Nothing, Nothing)
splitAxi4Stream (Just axi) = (orNothing aValid axiA, orNothing bValid axiB)
 where
  axiA = Axi4StreamM2S
    {_tdata = dataA
    , _tkeep = keepA
    , _tstrb = strbA
    , _tlast = lastA
    , _tid = _tid axi
    , _tdest = _tdest axi
    , _tuser = fst $ _tuser axi}

  axiB = Axi4StreamM2S
    {_tdata = dataB
    , _tkeep = keepB
    , _tstrb = strbB
    , _tlast = lastB
    , _tid = _tid axi
    , _tdest = _tdest axi
    , _tuser = snd $ _tuser axi}

  (dataA, dataB) = splitAtI $ _tdata axi
  (keepA, keepB) = splitAtI $ _tkeep axi
  (strbA, strbB) = splitAtI $ _tstrb axi

  -- An asserted last signal will be assigned to the "last" valid transaction
  lastA = _tlast axi && not bValid
  lastB = _tlast axi && bValid

  -- The first output is valid if:
  -- * At least one of the corresponding keep bits is set
  -- * None of the other keep bits are set.
  aValid = or keepA || lastA
  bValid = or keepB

-- | Extends an @Axi4StreamM2S@ with null bytes. The lower indices of the vectors containing
-- data, keep and strobe are copied from the input transaction. The upper indices are filled
-- with null bytes. The _tlast, _tid, _tdest and _tuser fields are passed through.
extendAxi ::
  forall widthA widthB idWith destWidth userType .
  ( KnownNat widthA
  , KnownNat widthB
  , KnownNat idWith
  , KnownNat destWidth
  ) =>
  Axi4StreamM2S ('Axi4StreamConfig widthA idWith destWidth) userType ->
  Axi4StreamM2S ('Axi4StreamConfig (widthA + widthB) idWith destWidth) userType
extendAxi axi = Axi4StreamM2S
  { _tdata = _tdata axi ++ repeat 0
  , _tkeep = _tkeep axi ++ repeat False
  , _tstrb = _tstrb axi ++ repeat False
  , _tlast = _tlast axi
  , _tid = _tid axi
  , _tdest = _tdest axi
  , _tuser = _tuser axi
  }

-- | Combines two Axi4StreamM2S into a single Axi4StreamM2S. The data, keep and strobe
-- vectors are concatenated. The first transaction must contain the lower part of the
-- data, the second transaction must contain the upper part of the data. If _tlast is
-- set in the first transaction, a second transaction is not allowed and the function
-- will return @Nothing@.
combineAxi4Stream ::
  forall widthA widthB idWidth destWidth userTypeA userTypeB.
  ( KnownNat widthA
  , KnownNat widthB
  , KnownNat idWidth
  , KnownNat destWidth
  , NFDataX userTypeA
  , NFDataX userTypeB
  ) =>
  -- | First Axi4Stream transaction, should contain the lower bytes.
  Maybe (Axi4StreamM2S ('Axi4StreamConfig widthA idWidth destWidth) userTypeA) ->
  -- | Second Axi4Stream transaction, should contain the upper bytes.
  Maybe (Axi4StreamM2S ('Axi4StreamConfig widthB idWidth destWidth) userTypeB) ->
  -- | Combined Axi4Stream transaction, or @Nothing@ if the transactions are not compatible.
  Maybe (Axi4StreamM2S ('Axi4StreamConfig (widthA + widthB) idWidth destWidth) (userTypeA, userTypeB))
combineAxi4Stream maybeAxiA maybeAxiB = case (maybeAxiA, maybeAxiB) of
  (Just axiA, Just axiB) -> orNothing compatibleAxis axiNew
   where
    axiNew = Axi4StreamM2S
      {_tdata = _tdata axiA ++ _tdata axiB
      , _tkeep = _tkeep axiA ++ _tkeep axiB
      , _tstrb = _tstrb axiA ++ _tstrb axiB
      , _tlast = _tlast axiB
      , _tid   = _tid   axiA
      , _tdest = _tdest axiA
      , _tuser = (_tuser axiA, _tuser axiB)
      }
    -- We can only combine two Axi4Streams if they have the same id, dest and the first
    -- transaction is not the end of a packet.
    compatibleAxis =
      _tid axiA == _tid axiB && _tdest axiA == _tdest axiB && not (_tlast axiA)

  (Just axi, Nothing) -> Just $ Axi4StreamM2S
    { _tdata = _tdata axi ++ repeat 0
    , _tkeep = _tkeep axi ++ repeat False
    , _tstrb = _tstrb axi ++ repeat False
    , _tlast = _tlast axi
    , _tid   = _tid   axi
    , _tdest = _tdest axi
    , _tuser = (_tuser axi, deepErrorX "combineAxi4Stream: Undefined second _tuser")
    }

  (Nothing, Just axi) -> Just $ Axi4StreamM2S
    { _tdata = repeat 0 ++ _tdata axi
    , _tkeep = repeat False ++ _tkeep axi
    , _tstrb = repeat False ++ _tstrb axi
    , _tlast = _tlast axi
    , _tid   = _tid   axi
    , _tdest = _tdest axi
    , _tuser = (deepErrorX "combineAxi4Stream: Undefined first _tuser", _tuser axi)
    }
  _ -> Nothing
axiUserMap ::
  forall userTypeA userTypeB conf.
  (userTypeA -> userTypeB) -> Axi4StreamM2S conf userTypeA -> Axi4StreamM2S conf userTypeB
axiUserMap f axi = axi{_tuser = f (_tuser axi)}

-- | A custom of `==` for Axi4StreamM2S that only checks the data bytes if they are valid.
-- TODO: We should make better use of ADTs in `Axi4StreamM2S` to allow us to use derived
-- typeclass instances.
eqAxi4Stream ::
  (Eq userType, KnownAxi4StreamConfig conf) =>
  Axi4StreamM2S conf userType ->
  Axi4StreamM2S conf userType -> Bool
eqAxi4Stream axiA axiB = lastSame && idSame && destSame && userSame && and keepsSame && and bytesValid
   where
    keepsSame = (==) <$> _tkeep axiA <*> _tkeep axiB
    lastSame = _tlast axiA == _tlast axiB
    idSame = _tid axiA == _tid axiB
    destSame = _tdest axiA == _tdest axiB
    userSame = _tuser axiA == _tuser axiB

    -- For all bytes where the keep is high, the data and strb must be the same.
    keeps = (||) <$> _tkeep axiA <*> _tkeep axiB
    dataSame = (==) <$> _tdata axiA <*> _tdata axiB
    strbSame = (==) <$> _tstrb axiA <*> _tstrb axiB
    bytesValid = zipWith3 (\ k d s -> (not k) || (d && s)) keeps strbSame dataSame

-- | Integrated logic analyzer for an Axi4Stream bus, it captures the data, keep, ready and last signals.
ilaAxi4Stream ::
  forall dom conf userType .
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
    ilaInst = ila
      (ilaConfig $
           "m2s_tdata"
        :> "m2s_tkeep"
        :> "m2s_tlast"
        :> "s2m_tready"
        :> Nil) { advancedTriggers = True, stages = stages0, depth = depth0 }
      hasClock
      (_tdata . fromJust <$> m2s)
      (_tkeep . fromJust <$> m2s)
      (_tlast . fromJust <$> m2s)
      (_tready <$> s2m)
  in
    ilaInst `hwSeqX` (s2m, m2s)

extendWithErrorX ::
  forall n m a . (KnownNat n, KnownNat m, NFDataX a) =>
  Vec n a -> Vec (n + m) a
extendWithErrorX = (++ deepErrorX "extendWithErrorX: Undefined")

dropUserAxi4Stream ::
  forall dom conf userType .
  ( KnownAxi4StreamConfig conf
  , HiddenClockResetEnable dom
  , NFDataX userType) =>
  Circuit
    (Axi4Stream dom conf userType)
    (Axi4Stream dom conf ())
dropUserAxi4Stream = DfConv.map Proxy Proxy (\axi -> axi{_tuser = ()})
