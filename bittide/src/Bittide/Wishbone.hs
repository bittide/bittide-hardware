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

unsafeToDf :: Circuit (CSignal dom (Maybe a)) (Df dom a)
unsafeToDf = Circuit $ \ (CSignal cSig, _) -> (CSignal $ pure (), maybeToData <$> cSig)

uartDf ::
  (HiddenClockResetEnable dom, ValidBaud dom baud) =>
  SNat baud ->
  Circuit
    (Df dom (BitVector 8), CSignal dom Bit)
    (CSignal dom (Maybe (BitVector 8)), CSignal dom Bit)
uartDf baud = Circuit go
 where
  go ((request, ~(CSignal rxBit)),_) = ((Ack <$> ack, CSignal $ pure ()), (CSignal received, CSignal txBit))
   where
    (received, txBit, ack) = uart baud rxBit (dataToMaybe <$> request)

uartWb ::
  forall dom addrW nBytes baudRate transmitBufferDepth receiveBufferDepth .
  ( HiddenClockResetEnable dom, ValidBaud dom baudRate
  , 1 <= transmitBufferDepth
  , 1 <= receiveBufferDepth
  , 2 <= addrW
  , KnownNat addrW
  , KnownNat nBytes, 1 <= nBytes
  ) =>
  SNat transmitBufferDepth ->
  SNat receiveBufferDepth ->
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

wbToDf ::
  forall dom addrW nBytes txFifoDepth .
  ( KnownNat nBytes, 1 <= nBytes, KnownNat addrW, 2 <= addrW) =>
  Circuit
    (Wishbone dom 'Standard addrW (Bytes nBytes), Df dom (BitVector 8), CSignal dom (FifoMeta txFifoDepth))
    (Df dom (BitVector 8), CSignal dom (Bool, Bool))
wbToDf = Circuit $
  bimap (third CSignal . unbundle) (second CSignal . unbundle) .
  unbundle .
  fmap go .
  bundle .
  bimap (bundle . third unCSignal) (bundle . second unCSignal)
 where
  third f (a, b, c) = (a, b, f c)
  unCSignal (CSignal s) = s
  go ((WishboneM2S{..}, dataToMaybe -> rxData, fifoFull -> txFull), (Ack txAck, _)) =
    ( ((emptyWishboneS2M @()){acknowledge, err, readData}, Ack validRead, ())
    , (txWrite, status)
    )
   where
    (alignedAddr, alignment) = split @_ @(addrW - 2) @2 addr
    internalAddr = bitCoerce $ resize alignedAddr :: Index 2
    addrLegal = alignedAddr <= 1 && alignment == 0
    masterActive = busCycle && strobe
    validRead = masterActive && internalAddr == 0 && not writeEnable
    validWrite = masterActive && internalAddr == 0 && writeEnable && txAck
    validReadStatus = masterActive && internalAddr == 1 && not writeEnable

    acknowledge = (validRead || validWrite || validReadStatus) && addrLegal
    err = masterActive && (not (validRead || validWrite || validReadStatus) || not addrLegal)
    rxEmpty = not $ isJust rxData
    status = (rxEmpty, txFull)
    readData
      | validRead       = resize $ fromMaybe 0 rxData
      | validReadStatus = resize $ pack status
      | otherwise       = deepErrorX
        [i|"uartWb: Invalid request.
        BUS: {busCycle}
        STR: {strobe}
        ADDR: {addr}
        WE:{writeEnable}
        ACK:{acknowledge}
        ERR:{err}"|]

    txWrite
      | validWrite = Data $ resize writeData
      | otherwise  = NoData

-- | State record for the FIFO circuit.
data FifoState depth = FifoState
  { readPointer   :: Index depth
  , dataCount     :: Index (depth + 1)
  } deriving (Generic, NFDataX)

-- | Meta information from `fifoWithMeta`.
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
  -- | Consumes `Df dom a`, produces `Df dom a` along with ready signal and data count.
    Circuit (Df dom a) (Df dom a, CSignal dom (FifoMeta depth))
fifoWithMeta depth@SNat = Circuit circuitFunction
 where
  circuitFunction (fifoIn, (readyIn, _)) = (Ack <$> readyOut, (fifoOut, CSignal fifoMeta))
   where
    circuitActive = unsafeToLowPolarity hasReset .&&. fromEnable hasEnable
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
