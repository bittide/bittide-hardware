-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -fconstraint-solver-iterations=100 #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}
module Bittide.Wishbone where

import Clash.Prelude

import Bittide.Arithmetic.Time(DomainFrequency)
import Bittide.SharedTypes

import Clash.Cores.UART (uart, ValidBaud)
import Clash.Cores.Experimental.I2C
import Clash.Cores.Xilinx.Ila (ila, ilaConfig, IlaConfig(..), Depth (..))
import Clash.Sized.Vector.ToTuple
import Clash.Util.Interpolate

import Bittide.DoubleBufferedRam (RegisterWritePriority(..), registerWb)
import Bittide.Extra.Maybe
import Clash.Functor.Extra
import Data.Bifunctor
import Data.Bool(bool)
import Data.Constraint.Nat.Extra
import Data.Maybe

import Protocols
import Protocols.Wishbone

import qualified Clash.Cores.Experimental.I2C.ByteMaster as I2C
import qualified Protocols.Df as Df
import qualified Protocols.Wishbone as Wishbone

{- $setup
>>> import Clash.Prelude
-}

-- Applying this hint yields a compile error
{-# ANN module "HLint: ignore Functor law" #-}

-- | A vector of base addresses, one for each slave.
type MemoryMap nSlaves = Vec nSlaves (Unsigned (CLog 2 nSlaves))

-- | Size of a bus that results from a `singleMasterInterconnect` with `nSlaves` slaves.
type MappedBusAddrWidth addr nSlaves = addr - CLog 2 nSlaves

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
  (Vec nSlaves (Wishbone dom 'Standard (MappedBusAddrWidth addrW nSlaves) a))
singleMasterInterconnect (fmap pack -> config) =
  Circuit go
 where
  go (masterS, slavesS) =
    fmap unbundle . unbundle $ route <$> masterS <*> bundle slavesS

  route master@(WishboneM2S{..}) slaves = (toMaster, toSlaves)
   where
    oneHotOrZeroSelected = fmap (==addrIndex) config
    (addrIndex, newAddr) =
      split @_ @_ @(MappedBusAddrWidth addrW nSlaves) addr
    toSlaves =
      (\newStrobe -> (updateM2SAddr newAddr master){strobe = strobe && newStrobe})
      <$> oneHotOrZeroSelected
    toMaster
      | busCycle && strobe =
          foldMaybes
          emptyWishboneS2M{err=True} -- master tries to access unmapped memory
          (maskToMaybes slaves oneHotOrZeroSelected)
      | otherwise = emptyWishboneS2M

dupWb ::
  forall dom aw .
  (KnownDomain dom, KnownNat aw) =>
  Circuit
    (Wishbone dom 'Standard aw (Bytes 4))
    ( Wishbone dom 'Standard aw (Bytes 4)
    , ( CSignal dom (WishboneM2S aw 4 (Bytes 4))
      , CSignal dom (WishboneS2M (Bytes 4))
      )
    )
dupWb = Circuit go
  where
    go (m2s0, (s2m0, _)) =
      (s2m0, (m2s0, (m2s0, s2m0)))

-- | An ILA monitoring all M2S and S2M signals on a Wishbone bus. Installs two
-- extra signals 'capture' and 'trigger' that can be used as defaults for triggering
-- the ILA and conditional capturing. Trigger will be active for every valid
-- transaction, while capture will be active for as long as trigger and a cycle
-- after it.
ilaWb ::
  forall dom addrW a .
  HiddenClock dom =>
  -- | Number of registers to insert at each probe. Supported values: 0-6.
  -- Corresponds to @C_INPUT_PIPE_STAGES@. Default is @0@.
  Index 7 ->
  -- | Number of samples to store. Corresponds to @C_DATA_DEPTH@. Default set
  -- by 'ilaConfig' equals 'D4096'.
  Depth ->
  Circuit
    (Wishbone dom 'Standard addrW a)
    (Wishbone dom 'Standard addrW a)
ilaWb stages0 depth0 = Circuit $ \(m2s, s2m) ->
  let
    -- Our TCL infrastructure looks for 'trigger' and 'capture' and uses it to
    -- trigger the ILA and do selective capture. Though defaults are changable
    -- using Vivado, we set it to capture only valid Wishbone transactions plus
    -- a single cycle after it.
    trigger = Wishbone.strobe <$> m2s .&&. Wishbone.busCycle <$> m2s
    capture = trigger .||. dflipflop trigger

    ilaInst :: Signal dom ()
    ilaInst = ila
      ((ilaConfig $
           "m2s_addr"
        :> "m2s_writeData"
        :> "m2s_busSelect"
        :> "m2s_busCycle"
        :> "m2s_strobe"
        :> "m2s_writeEnable"
        :> "s2m_readData"
        :> "s2m_acknowledge"
        :> "s2m_err"
        :> "s2m_stall"
        :> "s2m_retry"
        :> "capture"
        :> "trigger"
        :> Nil) { advancedTriggers = True, stages = stages0, depth = depth0 })
      hasClock
      (Wishbone.addr        <$> m2s)
      (Wishbone.writeData   <$> m2s)
      (Wishbone.busSelect   <$> m2s)
      (Wishbone.busCycle    <$> m2s)
      (Wishbone.strobe      <$> m2s)
      (Wishbone.writeEnable <$> m2s)
      (Wishbone.readData    <$> s2m)
      (Wishbone.acknowledge <$> s2m)
      (Wishbone.err         <$> s2m)
      (Wishbone.stall       <$> s2m)
      (Wishbone.retry       <$> s2m)
      capture
      trigger
  in
    ilaInst `hwSeqX` (s2m, m2s)

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
 , Signal dom (Vec nSlaves (WishboneM2S (MappedBusAddrWidth addrW nSlaves) (Regs a 8) a)))
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
unsafeToDf = Circuit $ \ (cSig, _) -> (pure (), Df.maybeToData <$> cSig)

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
  go ((request, rxBit),_) =
    ( (Ack <$> ack, pure ())
    , (received, txBit) )
   where
    (received, txBit, ack) = uart baud rxBit (Df.dataToMaybe <$> request)

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
    bimap unbundle unbundle .
    unbundle .
    fmap go .
    bundle .
    bimap bundle bundle
   where
    go ((WishboneM2S{..}, Df.dataToMaybe -> rxData, fifoFull -> txFull), (Ack txAck, _))
      -- not in cycle
      | not (busCycle && strobe)
      = ( ((emptyWishboneS2M @()) { readData = invalidReq }, Ack False, ())
        , (Df.NoData, status)
        )
      -- illegal addr
      | not addrLegal
      = ( ((emptyWishboneS2M @()) { err = True, readData = invalidReq }, Ack False, ())
        , (Df.NoData, status)
        )
      -- read at 0
      | not writeEnable && internalAddr == 0 =
        ( ( (emptyWishboneS2M @())
            {acknowledge = True, readData = resize $ fromMaybe 0 rxData}, Ack True, ())
          , (Df.NoData, status)
        )
      -- write at 0
      | writeEnable && internalAddr == 0 =
        ( ( (emptyWishboneS2M @())
            {acknowledge = txAck , readData = invalidReq}, Ack False, ())
          , (Df.Data $ resize writeData, status)
        )
      -- read at 1
      | not writeEnable && internalAddr == 1 =
        ( ( (emptyWishboneS2M @())
            {acknowledge = True, readData = resize $ pack status}, Ack False, ())
          , (Df.NoData, status)
        )
      | otherwise = ((emptyWishboneS2M { err = True }, Ack False, ()), (Df.NoData, status))
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
  circuitFunction (fifoIn, (readyIn, _)) = (Ack <$> readyOut, (fifoOut, fifoMeta))
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
    (Bool, Df.Data a, Ack, a) ->
    ( FifoState depth
    , (Index depth, Maybe (Index depth, a), Df.Data a, Bool, FifoMeta depth))
  go state@FifoState{..} (False, _, _,_) = (state,(readPointer, Nothing, Df.NoData, False, fifoMeta))
   where
    fifoEmpty = dataCount == 0
    fifoFull = dataCount == maxBound
    fifoMeta = FifoMeta {fifoEmpty, fifoFull, fifoDataCount = dataCount}
  go FifoState{..} (True, Df.dataToMaybe -> fifoIn, Ack readyIn, bramOut) = (nextState, output)
   where
    fifoEmpty = dataCount == 0
    fifoFull = dataCount == maxBound
    writePointer = satAdd SatWrap readPointer $ resize dataCount

    readSuccess = not fifoEmpty && readyIn
    writeSuccess = not fifoFull && isJust fifoIn

    readPointerNext = if readSuccess then satSucc SatWrap readPointer else readPointer
    writeOpGo = if writeSuccess then (writePointer,) <$> fifoIn else Nothing
    fifoOutGo = if fifoEmpty then Df.NoData else Df.Data bramOut

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
  -- | Read acknowledgement ->
  Bool ->
  -- | Wishbone bus (master to slave)
  WishboneM2S addrW nBytes (Bytes nBytes) ->
  -- |
  -- 1. Written data
  -- 2. Transaction address
  -- 2. Outgoing wishbone bus (slave to master)
  ( Vec nRegisters (Maybe (Bytes nBytes))
  , Maybe (Index nRegisters)
  , WishboneS2M (Bytes nBytes))
wbToVec readableData readAck WishboneM2S{..} = (writtenData, transAddr, wbS2M)
 where
  (alignedAddress, alignment) = split @_ @(addrW - 2) @2 addr
  addressRange = maxBound :: Index nRegisters
  invalidAddress = (alignedAddress > resize (pack addressRange)) || alignment /= 0
  masterActive = strobe && busCycle
  err = masterActive && invalidAddress
  acknowledge = masterActive && not err && readAck
  wbWriting = writeEnable && masterActive && not err
  wbAddr = unpack $ resize alignedAddress :: Index nRegisters
  transAddr = orNothing (masterActive && not err) wbAddr
  readData = readableData !! wbAddr
  writtenData
    | wbWriting = replace wbAddr (Just writeData) (repeat Nothing)
    | otherwise = repeat Nothing
  wbS2M = (emptyWishboneS2M @(Bytes 4)){acknowledge, readData, err}

-- | Wishbone accessible circuit that contains a free running 64 bit counter. We can
-- observe this counter to get a sense of time, overflows should be accounted for by
-- the master.
timeWb ::
  forall dom addrW .
  ( HiddenClockResetEnable dom
  , KnownNat addrW
  , 2 <= addrW
  , 1 <= DomainPeriod dom) =>
  Circuit (Wishbone dom 'Standard addrW (Bytes 4)) ()
timeWb = Circuit $ \(wbM2S, _) -> (mealy goMealy (0,0) wbM2S, ())
 where
  goMealy (frozen, count :: Unsigned 64) wbM2S = ((nextFrozen, succ count), wbS2M)
       where
    freq = natToNum @(DomainFrequency dom) :: Unsigned 64
    nextFrozen = if isJust (head writes) then count else frozen
    RegisterBank (splitAtI -> (frozenMsbs, frozenLsbs)) = getRegsBe @8 frozen
    RegisterBank (splitAtI -> (freqMsbs, freqLsbs)) = getRegsBe @8 freq
    (writes, _, wbS2M) = wbToVec
      (0 :> fmap pack (frozenLsbs :> frozenMsbs :> freqLsbs :> freqMsbs :> Nil)) True wbM2S

i2cWb ::
  forall dom addrW nBytes .
  ( HiddenClockResetEnable dom
  , 2 <= addrW
  , KnownNat addrW
  , KnownNat nBytes, 1 <= nBytes
  ) =>
  Circuit
    (Wishbone dom 'Standard addrW (Bytes nBytes), CSignal dom (Bit, Bit))
    (CSignal dom (Maybe Bit, Maybe Bit))
i2cWb = case (cancelMulDiv @nBytes @8) of
  Dict -> Circuit go
    where
      go ((wbM2S, i2cIn), _) =  i2cWbIla `hwSeqX` ((wbS2M, pure ()), i2cOut)
       where
        -- Wishbone interface consists of:
        -- 0. i2c data Read-Write
        -- 1. clock divider Read-Write
        -- 2. flags: MSBs are Read-Only flags, LSBs are Read-Write flags.
        (vecOut, transAddr, wbS2M) = unbundle $ wbToVec <$> bundle vecIn <*> wbAck <*> wbM2S
        vecIn = fmap resize dOut :> fmap (resize . pack) clkDiv :> flagsRead :> Nil
        (i2cWrite, clkDivWrite, flagsWrite) = (vecToTuple . unbundle) vecOut

        -- busy is the only Read-only flag, other flags are Read-Write.
        flagsRead = resize . pack <$> bundle (busy, rwFlagsReg)
        rwFlagsWrite :: Signal dom (Maybe (Vec 5 Bool))
        rwFlagsWrite = (unpack . resize) <<$>> flagsWrite

        rwFlagsReg = regMaybe
          (False :> False :> False :> False :> True :> Nil)
          rwFlagsRegNext
        (_, ackOutReg, ackIn, claimBus, smReset) = (vecToTuple . unbundle) rwFlagsReg

        -- ReadWrite flags
        rwFlagsRegSetters = bundle $ al :> (mux hostAck (fmap not ackOut) ackOutReg) :> ackIn :> claimBus :> smReset :> Nil

        -- Alternative of wishbone write and updated i2c status signals.
        rwFlagsRegNext = (<|>) <$> rwFlagsWrite <*>
          (orNothing <$> (al .||. hostAck) <*> (zipWith (||) <$> rwFlagsReg <*> rwFlagsRegSetters))

        clkDiv = regMaybe maxBound (unpack . resize <<$>> clkDivWrite)

        -- Alternative based on i2cWrite and transAddr
        i2cOp = mux hostAck (pure Nothing) $ (<|>)
          <$> ((I2C.WriteData . resize) <<$>> i2cWrite)
          <*> (flip orNothing I2C.ReadData <$> (transAddr .==. pure (Just 0)))

        -- If the wishbone interface targets the i2c core, wait for acknowledgement.
        wbAck = (pure (Just 0) ./=. transAddr) .||. hostAck
        (dOut,hostAck,busy,al,ackOut,i2cOut) =
          i2c hasClock hasReset smReset (fromEnable hasEnable) clkDiv claimBus i2cOp ackIn i2cIn

        onChange :: (HiddenClockResetEnable dom, Eq a, NFDataX a) => Signal dom a -> Signal dom Bool
        onChange x = (Just <$> x) ./=. register Nothing (Just <$> x)

        capture :: Signal dom Bool
        capture = withClockResetEnable hasClock hasReset enableGen $
          onChange $ bundle
            ( isJust <$> i2cOp
            , wbAck
            , flagsRead
            , isJust <$> transAddr
            , hostAck
            , busy
            , al
            , ackOut
            , ackIn
            , claimBus
            )

        i2cWbIla :: Signal dom ()
        i2cWbIla = setName @"i2cWbIla" $ ila
          ((ilaConfig $
               "trigger_2"
            :> "capture_2"
            :> "i2cOp"
            :> "wbAck"
            :> "flagsRead"
            :> "transAddr"
            :> "hostAck"
            :> "busy"
            :> "al"
            :> "ackOut"
            :> "ackIn"
            :> "claimBus"
            :> Nil
          ) { depth = D16384 })
          hasClock
          (pure True :: Signal dom Bool)
          capture
          -- Debug signals
          i2cOp
          wbAck
          flagsRead
          transAddr
          hostAck
          busy
          al
          ackOut
          ackIn
          claimBus

-- Wishbone accessible register circuit which can only be written to from the circuit.
statusRegWb ::
  forall dom a nBytes addrW .
  ( HiddenClockResetEnable dom
  , BitPack a, NFDataX a
  , KnownNat nBytes, 1 <= nBytes
  , KnownNat addrW, 2 <= addrW
  ) =>
  a ->
  Circuit
    (CSignal dom (Maybe a), Wishbone dom 'Standard addrW (Bytes nBytes))
    ()
statusRegWb initVal = case (cancelMulDiv @nBytes @8) of
  Dict -> fromSignals go
   where
    go ((inp,wbM2S), _) = ((pure (), wbS2M), ())
     where
       (_, wbS2M) = registerWb CircuitPriority initVal wbM2S inp

-- Wishbone accessible register circuit which can only be written to by the wishbone bus.
controlRegWb ::
  forall dom a nBytes addrW .
  ( HiddenClockResetEnable dom
  , BitPack a, NFDataX a
  , KnownNat nBytes, 1 <= nBytes
  , KnownNat addrW, 2 <= addrW
  ) =>
  a ->
  Circuit
    (Wishbone dom 'Standard addrW (Bytes nBytes))
    (CSignal dom a)
controlRegWb initVal = case (cancelMulDiv @nBytes @8) of
  Dict -> fromSignals go
   where
    go (wbM2S, _) = (wbS2M, a)
     where
       (a, wbS2M) = registerWb WishbonePriority initVal wbM2S (pure Nothing)
