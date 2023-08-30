-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}
module Bittide.Wishbone where

import Clash.Prelude

import Clash.Cores.UART(uart, ValidBaud)
import Data.Constraint (Dict(Dict))
import Protocols.Df hiding (zipWith, pure, bimap, first, second, route)
import Protocols.Internal
import Protocols.Wishbone
import Data.Bifunctor

import Bittide.SharedTypes
import Data.Bool(bool)
import Data.Constraint.Nat.Extra (divWithRemainder)
import Data.Maybe
import Clash.Util.Interpolate


{- $setup
>>> import Clash.Prelude
-}

-- Applying this hint yields a compile error
{-# ANN module "HLint: ignore Functor law" #-}

-- | A vector of base addresses, one for each slave.
type MemoryMap nSlaves = Vec nSlaves (Index nSlaves)

-- | Size of a bus that results from a `singleMasterInterconnect` with `nSlaves` slaves.
type MappedBus addr nSlaves = addr - CLog 2 nSlaves

{-# NOINLINE singleMasterInterconnect #-}
-- | Component that maps multiple slave devices to a single master device over the wishbone
-- bus. It routes the incoming control signals to a slave device based on the 'MemoryMap',
-- a vector of base addresses.
singleMasterInterconnect ::
 forall dom nSlaves addrW a .
 ( HiddenClockResetEnable dom
 , KnownNat nSlaves, 1 <= nSlaves
 , KnownNat addrW, (CLog 2 nSlaves <= addrW)
 , BitPack a
 , NFDataX a) =>
 MemoryMap nSlaves ->
 Circuit
  (Wishbone dom 'Standard addrW a)
  (Vec nSlaves (Wishbone dom 'Standard (MappedBus addrW nSlaves) a))
singleMasterInterconnect (fmap pack -> config) =
  Circuit go
 where
  go (masterS, slavesS) =
    fmap unbundle . unbundle $ route <$> masterS <*> bundle slavesS

  route master@(WishboneM2S{..}) slaves = (toMaster, toSlaves)
   where
    oneHotSelected = fmap (==addrIndex) config
    (addrIndex, newAddr) =
      split @_ @(BitSize (Index nSlaves)) @(MappedBus addrW nSlaves) addr
    toSlaves =
      (\newStrobe -> (updateM2SAddr newAddr master){strobe = strobe && newStrobe})
      <$> oneHotSelected
    toMaster
      | busCycle && strobe = foldMaybes emptyWishboneS2M (maskToMaybes slaves oneHotSelected)
      | otherwise = emptyWishboneS2M

-- | Given a vector with elements and a mask, promote all values with a corresponding
-- 'True' to 'Just', others to 'Nothing'.
--
-- Example:
--
-- >>> maskToMaybes ('a' :> 'b' :> Nil) (True :> False :> Nil)
-- Just 'a' :> Nothing :> Nil
--
maskToMaybes :: Vec n a -> Vec n Bool -> Vec n (Maybe a)
maskToMaybes = zipWith (bool Nothing . Just)

-- | Fold 'Maybe's to a single value. If the given vector does not contain any 'Just',
-- the default value is picked. Prefers the leftmost value when the vector contains
-- multiple 'Just's.
--
-- Example:
--
-- >>> foldMaybes 'a' (Nothing :> Just 'c' :> Nil)
-- 'c'
-- >>> foldMaybes 'a' (Just 'b' :> Just 'c' :> Nil)
-- 'b'
-- >>> foldMaybes 'a' (Nothing :> Nothing :> Nil)
-- 'a'
--
foldMaybes :: a -> Vec n (Maybe a) -> a
foldMaybes a Nil = a
foldMaybes dflt v@(Cons _ _) = fromMaybe dflt $ fold (<|>) v

-- | Version of 'singleMasterInterconnect' that does not use the 'Circuit' abstraction
-- from @clash-protocols@ but exposes 'Signal's directly.
singleMasterInterconnect' ::
 forall dom nSlaves addrW a .
 ( HiddenClockResetEnable dom
 , KnownNat nSlaves, 1 <= nSlaves
 , KnownNat addrW, CLog 2 nSlaves <= addrW
 , BitPack a
 , NFDataX a) =>
 MemoryMap nSlaves ->
 Signal dom (WishboneM2S addrW (Regs a 8) a) ->
 Signal dom (Vec nSlaves (WishboneS2M a)) ->
 ( Signal dom (WishboneS2M a)
 , Signal dom (Vec nSlaves (WishboneM2S (MappedBus addrW nSlaves) (Regs a 8) a)))
singleMasterInterconnect' config master slaves = (toMaster, bundle toSlaves)
 where
  Circuit f = singleMasterInterconnect @dom @nSlaves @addrW @a config
  (toMaster, toSlaves) =
    case divWithRemainder @(Regs a 8) @8 @7 of
      Dict ->
        f (master, unbundle slaves)

-- | Takes an input that features no back pressure mechanism and turn it into `Df`.
-- This function is unsafe because data can be lost when the input is @Just _@ and
-- the receiving circuit tries to apply back pressure.
unsafeToDf :: Circuit (CSignal dom (Maybe a)) (Df dom a)
unsafeToDf = Circuit $ \ (CSignal cSig, _) -> (CSignal $ pure (), maybeToData <$> cSig)

-- | 'Df' version of 'uart'.
uartDf ::
  (HiddenClockResetEnable dom, ValidBaud dom baud) =>
  SNat baud ->
  -- | Left side of circuit: word to send, receive bit
  -- Right side of circuit: received word, transmit bit
  Circuit
    ( Df dom (BitVector 8)
    , CSignal dom Bit
    )
    ( CSignal dom (Maybe (BitVector 8))
    , CSignal dom Bit
    )
uartDf baud = Circuit go
 where
  go ((request, ~(CSignal rxBit)),_) =
    ( (Ack <$> ack, CSignal $ pure ())
    , (CSignal received, CSignal txBit) )
   where
    (received, txBit, ack) = uart baud rxBit (dataToMaybe <$> request)

-- | Wishbone accessible UART interface with configurable FIFO buffers.
--   It takes the depths of the transmit and receive buffers and the baud rate as parameters.
--   The function returns a 'Circuit' with a 'Wishbone' interface and a 'CSignal' for the UART
--   receive bit as inputs, and outputs a 'CSignal' for the UART transmit bit and a 'CSignal'
--   tuple indicating the status of the UART.
--
--   The register layout is as follows:
--   - Address 0 (BitVector 8): UART data register (read/write)
--   - Address 4 (BitVector 2): UART status register (read-only)
--     Relevant masks:
--     - 0b01: Transmit buffer full
--     - 0b10: Receive buffer empty
uartWb ::
  forall dom addrW nBytes baudRate transmitBufferDepth receiveBufferDepth .
  ( HiddenClockResetEnable dom, ValidBaud dom baudRate
  , 1 <= transmitBufferDepth
  , 1 <= receiveBufferDepth
  , 2 <= addrW
  , KnownNat addrW
  , KnownNat nBytes, 1 <= nBytes
  ) =>
  -- | Recommended value: 16. This seems to be a good balance between resource
  -- usage and usability.
  SNat transmitBufferDepth ->
  -- | Recommended value: 16. This seems to be a good balance between resource
  -- usage and usability.
  SNat receiveBufferDepth ->
  -- | Valid baud rates are constrained by @clash-cores@'s 'ValidBaud' constraint.
  SNat baudRate ->
  Circuit
    (Wishbone dom 'Standard addrW (Bytes nBytes), CSignal dom Bit)
    (CSignal dom Bit, CSignal dom (Bool, Bool))
uartWb txDepth@SNat rxDepth@SNat baud = circuit $ \(wb, uartRx) -> do
  (txFifoIn, uartStatus) <- wbToDf -< (wb, rxFifoOut, txFifoMeta)
  (txFifoOut, txFifoMeta) <- fifoWithMeta txDepth -< txFifoIn
  (rxFifoIn, uartTx) <- uartDf baud -< (txFifoOut, uartRx)
  (rxFifoOut, _rx') <- fifoWithMeta rxDepth <| unsafeToDf -< rxFifoIn
  idC -< (uartTx, uartStatus)
 where
  wbToDf ::
    Circuit
      ( Wishbone dom 'Standard addrW (Bytes nBytes)
      , Df dom (BitVector 8)
      , CSignal dom (FifoMeta txFifoDepth)
      )
      ( Df dom (BitVector 8)
      , CSignal dom (Bool, Bool) -- (rxEmpty, txFull)
      )
  wbToDf = Circuit $
    bimap (third CSignal . unbundle) (second CSignal . unbundle) .
    unbundle .
    fmap go .
    bundle .
    bimap (bundle . third unCSignal) (bundle . second unCSignal)
   where
    third f (a, b, c) = (a, b, f c)
    unCSignal (CSignal s) = s
    go ((WishboneM2S{..}, dataToMaybe -> rxData, fifoFull -> txFull), (Ack txAck, _))
      -- not in cycle
      | not (busCycle && strobe)
      = ( ((emptyWishboneS2M @()) { readData = invalidReq }, Ack False, ())
        , (NoData, status)
        )
      -- illegal addr
      | not addrLegal
      = ( ((emptyWishboneS2M @()) { err = True, readData = invalidReq }, Ack False, ())
        , (NoData, status)
        )
      -- read at 0
      | not writeEnable && internalAddr == 0 =
        ( ( (emptyWishboneS2M @())
            {acknowledge = True, readData = resize $ fromMaybe 0 rxData}, Ack True, ())
          , (NoData, status)
        )
      -- write at 0
      | writeEnable && internalAddr == 0 =
        ( ( (emptyWishboneS2M @())
            {acknowledge = txAck , readData = invalidReq}, Ack False, ())
          , (Data $ resize writeData, status)
        )
      -- read at 1
      | not writeEnable && internalAddr == 1 =
        ( ( (emptyWishboneS2M @())
            {acknowledge = True, readData = resize $ pack status}, Ack False, ())
          , (NoData, status)
        )
      | otherwise = ((emptyWishboneS2M { err = True }, Ack False, ()), (NoData, status))
     where
      (alignedAddr, alignment) = split @_ @(addrW - 2) @2 addr
      internalAddr = bitCoerce $ resize alignedAddr :: Index 2
      addrLegal = alignedAddr <= 1 && alignment == 0
      rxEmpty = not $ isJust rxData
      status = (rxEmpty, txFull)
      invalidReq = deepErrorX
        [i|uartWb: Invalid request.
          BUS: {busCycle}
          STR: {strobe}
          ADDR: {addr}
          WE:{writeEnable}
          ACK:{acknowledge}
          ERR:{err}|]


-- | State record for the FIFO circuit.
data FifoState depth = FifoState
  { readPointer   :: Index depth
  , dataCount     :: Index (depth + 1)
  } deriving (Generic, NFDataX)

-- | Meta information from 'fifoWithMeta'.
data FifoMeta depth = FifoMeta
  { fifoEmpty     :: Bool
  , fifoFull      :: Bool
  , fifoDataCount :: Index (depth + 1)
  } deriving (Generic, NFDataX)

-- | A generic First-In-First-Out (FIFO) circuit with a specified depth that exposes
-- meta information such as in `FifoMeta`. At least one cycle latency.
-- When the reset is high or the enable is low, there will be no outgoing transactions and
-- incoming transactions are not acknowledged.
fifoWithMeta ::
  forall dom a depth .
  (HiddenClockResetEnable dom,  1 <= depth, NFDataX a) =>
  -- | The depth of the FIFO, should be at least 1.
  SNat depth ->
  -- | Consumes @Df dom a@, produces @Df dom a@ along with ready signal and data count.
  Circuit (Df dom a) (Df dom a, CSignal dom (FifoMeta depth))
fifoWithMeta depth@SNat = Circuit circuitFunction
 where
  circuitFunction (fifoIn, (readyIn, _)) = (Ack <$> readyOut, (fifoOut, CSignal fifoMeta))
   where
    circuitActive = unsafeToActiveLow hasReset .&&. fromEnable hasEnable
    bramOut =
      readNew (blockRamU NoClearOnReset depth (errorX "No reset function"))
      readAddr writeOp
    (readAddr, writeOp, fifoOut, readyOut, fifoMeta) =
      mealyB go initialState (circuitActive, fifoIn, readyIn, bramOut)

  -- Initial state of the FIFO
  initialState = FifoState
    { readPointer  = 0
    , dataCount    = 0
    }
  go ::
    FifoState depth ->
    (Bool, Data a, Ack, a) ->
    ( FifoState depth
    , (Index depth, Maybe (Index depth, a), Data a, Bool, FifoMeta depth))
  go state@FifoState{..} (False, _, _,_) = (state,(readPointer, Nothing, NoData, False, fifoMeta))
   where
    fifoEmpty = dataCount == 0
    fifoFull = dataCount == maxBound
    fifoMeta = FifoMeta {fifoEmpty, fifoFull, fifoDataCount = dataCount}
  go FifoState{..} (True, dataToMaybe -> fifoIn, Ack readyIn, bramOut) = (nextState, output)
   where
    fifoEmpty = dataCount == 0
    fifoFull = dataCount == maxBound
    writePointer = satAdd SatWrap readPointer $ resize dataCount

    readSuccess = not fifoEmpty && readyIn
    writeSuccess = not fifoFull && isJust fifoIn

    readPointerNext = if readSuccess then satSucc SatWrap readPointer else readPointer
    writeOpGo = if writeSuccess then (writePointer,) <$> fifoIn else Nothing
    fifoOutGo = if fifoEmpty then NoData else Data bramOut

    dataCountNext = dataCountDx dataCount
    dataCountDx = case (writeSuccess, readSuccess) of
      (True, False) -> satSucc SatError
      (False, True) -> satPred SatError
      _             -> id

    nextState = FifoState
      { readPointer  = readPointerNext
      , dataCount    = dataCountNext
      }

    fifoMeta = FifoMeta {fifoEmpty, fifoFull, fifoDataCount = dataCount}
    output = (readPointerNext, writeOpGo, fifoOutGo, not fifoFull, fifoMeta)

-- | Transforms a wishbone interface into a vector based interface.
-- Write operations will produce a 'Just (Bytes nBytes)' on the index corresponding
-- to the word-aligned Wishbone address.
-- Read operations will read from the index corresponding to the world-aligned
-- Wishbone address.
wbToVec ::
  forall dom nBytes addrW nRegisters .
  ( HiddenClockResetEnable dom
  , KnownNat nBytes
  , 1 <= nBytes
  , KnownNat addrW
  , 2 <= addrW
  , KnownNat nRegisters
  , 1 <= nRegisters) =>
  -- | Readable data.
  Vec nRegisters (Bytes nBytes) ->
  -- | Wishbone bus (master to slave)
  WishboneM2S addrW nBytes (Bytes nBytes) ->
  -- |
  -- 1. Written data
  -- 2. Outgoing wishbone bus (slave to master)
  ( Vec nRegisters (Maybe (Bytes nBytes))
  , WishboneS2M (Bytes nBytes))
wbToVec readableData WishboneM2S{..} = (writtenData, wbS2M)
 where
  (alignedAddress, alignment) = split @_ @(addrW - 2) @2 addr
  addressRange = maxBound :: Index nRegisters
  invalidAddress = (alignedAddress > resize (pack addressRange)) || alignment /= 0
  masterActive = strobe && busCycle
  err = masterActive && invalidAddress
  acknowledge = masterActive && not err
  wbWriting = writeEnable && acknowledge
  wbAddr = unpack $ resize alignedAddress :: Index nRegisters
  readData = readableData !! wbAddr
  writtenData
    | wbWriting = replace wbAddr (Just writeData) (repeat Nothing)
    | otherwise = repeat Nothing
  wbS2M = (emptyWishboneS2M @(Bytes 4)){acknowledge, readData, err}
