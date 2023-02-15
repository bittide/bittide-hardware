-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Bittide.Axi4 where

import Clash.Prelude

import Clash.Sized.Internal.BitVector (popCountBV)
import Data.Constraint
import Data.Maybe
import Data.Proxy
import Protocols
import Protocols.Axi4.Stream
import Protocols.Wishbone

import Bittide.Extra.Maybe
import Bittide.SharedTypes
import Data.Constraint.Nat.Extra
import qualified Protocols.DfConv as DfConv

{- $setup
>>> import Clash.Prelude
-}

-- | An Axi4Stream in with no gaps in the data. For each transaction the following holds:
-- * For a transaction that is not the last in a packet, all _tkeep bits are set.
-- * For a transaction that is the last in a packet, the _tlast bit is set and the
-- first n bits of _tkeep are set, where n is the number of bytes in the transaction.
type PackedAxi4Stream dom conf userType = Axi4Stream dom conf userType

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
    (PackedAxi4Stream dom ('Axi4StreamConfig (addedWidth + 1) idWidth destWidth) userType)
axisFromByteStream = Circuit (mealyB go Nothing)
 where
  go axiStored (axiIn, Axi4StreamS2M{_tready}) = (axiNext, (lhsS2M, rhsM2S))
   where

    -- Try to append the incoming axi to the stored axi
    combinedAxi = concatAxi axiStored axiIn
    inpValid = isJust axiIn && isJust combinedAxi
    inpInvalid = isJust axiIn && isNothing combinedAxi

    -- Shifting bytes
    axiPreShift = combinedAxi <|> concatAxi axiStored Nothing
    axiBytes =
      zipWith3 (\d k s -> orNothing k (d, s))
      (maybe (repeat 0) _tdata axiPreShift)
      (maybe (repeat False) _tkeep axiPreShift)
      (maybe (repeat False) _tstrb axiPreShift)

    (newData, newKeeps, newStrobes) =
      unzip3 $ (\m -> (maybe 0 fst m, isJust m, maybe False snd m)) <$> shiftPackVec axiBytes

    axiPostShift =
      (\a -> a{ _tdata = newData, _tkeep = newKeeps, _tstrb = newStrobes}) <$> axiPreShift

    shiftingDone = fromMaybe False
      $ (\pre post -> _tkeep pre == _tkeep post) <$> axiPreShift <*> axiPostShift

    -- Axi RHS
    -- The rhs axi is valid if we can not shift in more bytes and one of the following holds:
    -- * All keep bits of the output are set
    -- * We can not combine the lhs axi with the stored axi
    -- * The last bit of the output is set.
    rhsValid = shiftingDone && (and newKeeps || inpInvalid || maybe False _tlast axiPostShift)
    rhsM2S
      | rhsValid = axiPostShift
      | otherwise   = Nothing
    rhsHandshake = rhsValid && _tready

    -- Axi LHS
    -- The lhs is acknowledged if we can combine it with the stored axi and one
    -- of the following holds:
    -- * The rhs is not valid yet
    -- * THe rhs is valid and ready
    lhsReady = inpValid && (rhsHandshake || not rhsValid)
    lhsS2M = Axi4StreamS2M{_tready = lhsReady}

    -- State update
    axiNext
      | rhsHandshake = Nothing
      | otherwise    = fst $ splitAxi @_ @1 axiPostShift

-- | Shifts all @Just a@ in a `Vec n (Maybe a)` in the direction of head by one position.
-- The first element is replaced by @Nothing@.
-- >>> shiftPackVec (Just 1 :> Just 2 :> Nothing :> Just 3 :> Nothing :> Nothing :> Nil)
-- Just 1 :> Just 2 :> Just 3 :> Nothing :> Nothing :> Nothing :> Nil
shiftPackVec :: forall n a . (KnownNat n) => Vec (n + 1) (Maybe a) -> Vec (n + 1) (Maybe a)
shiftPackVec v = zipWith3 f canShift v (v <<+ Nothing)
 where
  f getNext current next = if getNext then next else current
  availables = fmap isNothing v
  canShift = scanl (||) (head availables) (tail availables)

-- | Transforms an Axi4 stream of /n/ bytes wide into an Axi4 stream of 1 byte
-- wide. It stores the incoming stream and shifts it out one by one. The incoming stream is acknowledged when the last byte is
-- acknowledged by the outgoing stream. The '_tuser', '_tdest' and '_tid' are
-- blindly routed to the output.
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
  go axiStored (axiIn, Axi4StreamS2M{_tready}) = (axiNext, (bigS2M, rhsM2S))
   where
    (rhsM2S, axiRest) = splitAxi @1 (concatAxi axiStored Nothing)
    axiNext
      | isNothing rhsM2S || _tready = axiRest <|> axiIn
      | otherwise = axiStored
    bigS2M = Axi4StreamS2M{_tready = isNothing axiRest && (isNothing rhsM2S || _tready)}

type EndOfPacket = Bool
type BufferFull = Bool

data WbAxisRxBufferState fifoDepth nBytes = WbAxisRxBufferState
  { readingFifo :: Bool
  , packetLength :: Index (fifoDepth * nBytes + 1)
  , writeCounter :: Index fifoDepth
  , packetComplete :: Bool
  , bufferFull :: Bool
  } deriving (Generic, NFDataX, Show)

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
      in ((wbS2M, axiS2M), status)

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
  , DataWidth conf ~ nBytes
  , Show axiUserType) =>
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
    ~(WishboneM2S{..}, maybeAxisM2S, wbData, clearStatus)
    = (newState, output)
   where
    masterActive = busCycle && strobe
    (alignedAddress, alignment) = split @_ @(wbAddrW - 2) @2 addr

    packetLengthAddress = maxBound - 1
    statusAddress       = maxBound :: Index (fifoDepth + 2)
    internalAddress     = (bitCoerce $ resize alignedAddress) :: Index (fifoDepth + 2)

    err = masterActive && (alignment /= 0 || alignedAddress > resize (pack statusAddress))

    statusBV = pack (packetComplete, bufferFull)
    wbHandshake = masterActive && not err

    (readData, nextReadingFifo, wbAcknowledge) = case (masterActive, internalAddress) of
      (True, (== packetLengthAddress) -> True) -> (resize $ pack packetLength, False, wbHandshake)
      (True, (== statusAddress) -> True) -> (resize statusBV, False, wbHandshake)
      (True, _) -> (wbData, wbHandshake && not readingFifo, wbHandshake && readingFifo)
      (False, _) -> (deepErrorX "undefined", False, False)

    axisReady = not (packetComplete || bufferFull)
    axisHandshake = axisReady && isJust maybeAxisM2S

    output =
      ( (emptyWishboneS2M @(Bytes nBytes)){readData, err, acknowledge = wbAcknowledge}
      , Axi4StreamS2M axisReady
      , unpack . resize $ pack internalAddress
      , maybeAxisM2S >>= orNothing axisHandshake . (writeCounter,) . pack . reverse . _tdata
      , (packetComplete, bufferFull)
      )

    -- Next state
    (nextPacketComplete, nextBufferFull)
      | wbAcknowledge && writeEnable && internalAddress == statusAddress
        = unpack $ resize writeData
      | axisHandshake = (packetComplete || maybe False _tlast maybeAxisM2S, bufferFull || writeCounter == maxBound)
      | otherwise = unpack $ statusBV `xor` pack clearStatus

    nextWriteCounter
      | axisHandshake  = satSucc SatBound writeCounter
      | packetComplete || bufferFull = 0
      | otherwise      = writeCounter

    bytesInStream = maybe (0 :: Index (nBytes + 1)) (leToPlus @1 @nBytes popCountBV . pack ._tkeep) maybeAxisM2S

    nextPacketLength
      | wbAcknowledge && writeEnable && internalAddress == packetLengthAddress
        = unpack $ resize writeData
      | axisHandshake
        = satAdd SatBound packetLength (bitCoerce $ resize bytesInStream)
      | otherwise
        = packetLength

    newState = WbAxisRxBufferState
      { readingFifo = nextReadingFifo
      , packetLength = nextPacketLength
      , writeCounter = nextWriteCounter
      , packetComplete = nextPacketComplete
      , bufferFull = nextBufferFull
      }

data BufferState bufferSize = AwaitingData | BufferFull | PacketComplete (Index (bufferSize + 1))
  deriving (Generic, NFDataX, Show)

data ReadStateMachine bufferSize =
    Idle | ReadingPacketSize | ReadingPacket (Index (bufferSize + 1)) | ClearingPacketLength | ClearingStatus
  deriving (Generic, NFDataX, Show)

rxReadMasterC ::
  forall dom nBytes addrWidth bufferDepth .
  ( HiddenClockResetEnable dom
  , 1 <= bufferDepth * nBytes
  , 1 <= bufferDepth
  , KnownNat addrWidth
  , KnownNat nBytes) =>
  SNat bufferDepth ->
  Circuit
  ()
  (Wishbone dom 'Standard addrWidth (Bytes nBytes), Axi4Stream dom ('Axi4StreamConfig nBytes 0 0) ())
rxReadMasterC s = case cancelMulDiv @nBytes @8 of
  Dict -> fromSignals $ \ (_, bwd) -> ((), rxReadMaster s bwd)

-- | Circuit capable of reading the wishbone interface of @wbAxisRxBuffer@ and
-- extracting Axi packets. Mostly useful for verification, but can be synthesized.
rxReadMaster ::
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
rxReadMaster SNat = mealyB go (AwaitingData @(bufferDepth * nBytes), Idle)
   where
    go
      (bufState, readState :: ReadStateMachine (bufferDepth * nBytes))
      ~(WishboneS2M{..}, Axi4StreamS2M{..}) = (nextState, (wbM2S, axiM2S))
     where
      -- Driving wishbone signals
      (writeEnable, addr) = case readState of
        Idle                 -> (False, natToNum @((1 + bufferDepth) * 4))
        ReadingPacketSize    -> (False, natToNum @(bufferDepth * 4))
        ReadingPacket i      -> (False, resize (pack i))
        ClearingPacketLength -> (True, natToNum @(bufferDepth * 4))
        ClearingStatus       -> (True, natToNum @((1 + bufferDepth) * 4))

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
              packetSize = unpack $ resize readData
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

mkKeep :: forall m n . (KnownNat n, KnownNat m) => Index m -> Vec n Bool
mkKeep i
  | i < natToNum @n =  fmap (< checkedResize i) indicesI
  | otherwise = repeat True


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
  goCircuit (lhsM2S, fmap _tready -> rhsS2M) = (lhsS2M, rhsM2S)
   where
    lhsS2M = Axi4StreamS2M <$> (consumeAxi .&&. fifoReady)
    rhsM2S = mux produceFifo fifoOut (pure Nothing)
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
    (AxiPacketComplete _) -> (False, True)
    AxiPassThrough -> (True, True)

-- | Circuit for resizing an Axi4Stream, produces packed streams.
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
  go (axiBuffer, True) ~(axiIn :: axiType, s2mRight) = ((newStored, True), (s2mLeft, m2sRight))
   where

    -- Axi Right
    validRight = maybe False (and . _tkeep) outputBuffer || maybe False _tlast outputBuffer || inpInvalid
    m2sRight = if validRight then outputBuffer else Nothing

    -- Axi Left
    inpValid = not validRight && isJust axiIn && isJust combinedAxi
    inpInvalid = isJust axiIn && isNothing combinedAxi
    readyLeft = isNothing excessBuffer && inpValid
    s2mLeft = Axi4StreamS2M readyLeft
    handshakeLeft = isJust axiIn && readyLeft

    -- State update
    (outputBuffer, excessBuffer) = splitAxi axiBuffer
    combinedAxi = concatAxi outputBuffer axiIn
    newStored = case (validRight, _tready s2mRight, handshakeLeft) of
      (True, False,_) -> axiBuffer
      (True, True, False) -> concatAxi excessBuffer Nothing
      (_, _, False) ->  concatAxi outputBuffer Nothing
      (_, _, True) -> myPack <$> combinedAxi

    myPack = packAxi @('Axi4StreamConfig (dataWidth + dataWidth) idWidth destWidth)

-- | Function to move all keep, data and strobes in an Axi4Stream to the front of the vectors based on the
-- _tkeep field.
packAxi ::
  (KnownAxi4StreamConfig conf) =>
  Axi4StreamM2S conf userType -> Axi4StreamM2S conf userType
packAxi axi = rhsM2S
 where
  rhsM2S = axi{_tdata = newData, _tstrb = newStrobe, _tkeep = newKeep}
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

-- | Splits an Axi4StreamM2S into two Axi4StreamM2S. The first one contains all
-- The data, keep and strobe vectors are split into two Vectors, the first vector is
-- assigned to the first output, the second vector is assigned to the second output.
-- If _tlast is not set, the outputs are only `Just` if at least one of their `keep`
-- bits is set. If _tlast is set, the _tlast of the first output is set if the second
-- output is `Nothing`. If the second output is `Just`, the _tlast of the first output
-- is `False` and the _tlast of the second output is `True`.
splitAxi ::
  forall widthA widthB idWith destWidth userType .
  ( KnownNat widthA
  , KnownNat widthB
  ) =>
  Maybe (Axi4StreamM2S ('Axi4StreamConfig (widthA + widthB) idWith destWidth) userType) ->
  ( Maybe (Axi4StreamM2S ('Axi4StreamConfig widthA idWith destWidth) userType)
  , Maybe (Axi4StreamM2S ('Axi4StreamConfig widthB idWith destWidth) userType))
splitAxi Nothing = (Nothing, Nothing)
splitAxi (Just axi) = (orNothing aValid axiA, orNothing bValid axiB)
 where
  axiA = Axi4StreamM2S
    {_tdata = dataA
    , _tkeep = keepA
    , _tstrb = strbA
    , _tlast = lastA
    , _tid = _tid axi
    , _tdest = _tdest axi
    , _tuser = _tuser axi}

  axiB = Axi4StreamM2S
    {_tdata = dataB
    , _tkeep = keepB
    , _tstrb = strbB
    , _tlast = lastB
    , _tid = _tid axi
    , _tdest = _tdest axi
    , _tuser = _tuser axi}

  (dataA, dataB) = splitAtI $ _tdata axi
  (keepA, keepB) = splitAtI $ _tkeep axi
  (strbA, strbB) = splitAtI $ _tstrb axi
  aValid = aKeepBytes || lastA
  bValid = bKeepBytes
  (aKeepBytes, bKeepBytes) = (or keepA, or keepB)
  lastA = _tlast axi && not bKeepBytes
  lastB = _tlast axi && bKeepBytes

-- | Returns @True@ if we can combine the two Axi4StreamM2S into a single Axi4StreamM2S.
-- This is the case if the @_tid@, @_tdest@ and @_tuser@ are equal and the @_tlast@ of the first
-- Axi4StreamM2S is not set.
compatibleAxis ::
  forall widthA widthB destWidth idWidth userType .
  ( KnownNat destWidth, KnownNat idWidth, Eq userType) =>
  Axi4StreamM2S ('Axi4StreamConfig widthA idWidth destWidth) userType ->
  Axi4StreamM2S ('Axi4StreamConfig widthB idWidth destWidth) userType ->
  Bool
compatibleAxis axiA axiB =
  _tid axiA == _tid axiB &&
  _tdest axiA == _tdest axiB &&
  _tuser axiA == _tuser axiB &&
  not (_tlast axiA)

-- | Combines two Axi4StreamM2S into a single Axi4StreamM2S. The data, keep and strobe
-- vectors are concatenated. If both Axi4Streams are `Just`, it uses @compatibleAxis@ to
-- determine if we can concatenate them. The vectors of the axi streams are concatenated.
concatAxi ::
  forall widthA widthB idWidth destWidth userType.
  ( KnownNat widthA
  , KnownNat widthB
  , KnownNat idWidth
  , KnownNat destWidth
  , Eq userType
  ) =>
  Maybe (Axi4StreamM2S ('Axi4StreamConfig widthA idWidth destWidth) userType) ->
  Maybe (Axi4StreamM2S ('Axi4StreamConfig widthB idWidth destWidth) userType) ->
  Maybe (Axi4StreamM2S ('Axi4StreamConfig (widthA + widthB) idWidth destWidth) userType)
concatAxi maybeAxiA maybeAxiB = case (maybeAxiA, maybeAxiB) of
  (Just axiA, Just axiB) -> orNothing (compatibleAxis axiA axiB) axiNew
   where
    axiNew = Axi4StreamM2S
      {_tdata = _tdata axiA ++ _tdata axiB
      , _tkeep = _tkeep axiA ++ _tkeep axiB
      , _tstrb = _tstrb axiA ++ _tstrb axiB
      , _tlast = _tlast axiB
      , _tid   = _tid   axiA
      , _tdest = _tdest axiA
      , _tuser = _tuser axiA
      }
  (Just axi, Nothing) -> Just $ Axi4StreamM2S
    { _tdata = _tdata axi ++ repeat 0
    , _tkeep = _tkeep axi ++ repeat False
    , _tstrb = _tstrb axi ++ repeat False
    , _tlast = _tlast axi
    , _tid   = _tid   axi
    , _tdest = _tdest axi
    , _tuser = _tuser axi
    }
  (Nothing, Just axi) -> Just $ Axi4StreamM2S
    { _tdata = repeat 0 ++ _tdata axi
    , _tkeep = repeat False ++ _tkeep axi
    , _tstrb = repeat False ++ _tstrb axi
    , _tlast = _tlast axi
    , _tid   = _tid   axi
    , _tdest = _tdest axi
    , _tuser = _tuser axi
    }
  _ -> Nothing

-- | A custom of `==` for Axi4StreamM2S that only checks the data bytes if they are valid.
-- TODO: We should make better use of ADTs in `Axi4StreamM2S` to allow us to use derived
-- typeclass instances.
eqAxi ::
  (Eq userType, KnownAxi4StreamConfig conf) =>
  Axi4StreamM2S conf userType ->
  Axi4StreamM2S conf userType -> Bool
eqAxi axiA axiB = lastSame && idSame && destSame && userSame && and keepsSame && and bytesValid
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
