-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=100 #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

module Bittide.Wishbone where

import Clash.Prelude

import Bittide.DoubleBufferedRam
import Bittide.SharedTypes

import Clash.Cores.UART (ValidBaud, uart)
import Clash.Cores.Xilinx.Ila (Depth, IlaConfig (..), ila, ilaConfig)
import Clash.Cores.Xilinx.Unisim.DnaPortE2
import Clash.Util.Interpolate

import Data.Bifunctor
import Data.Bool (bool)
import Data.Constraint.Nat.Extra
import Data.Maybe

import Protocols
import Protocols.Wishbone

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

{- | Component that maps multiple slave devices to a single master device over the wishbone
bus. It routes the incoming control signals to a slave device based on the 'MemoryMap',
a vector of base addresses.
-}
singleMasterInterconnect ::
  forall dom nSlaves addrW a.
  ( HiddenClockResetEnable dom
  , KnownNat nSlaves
  , 1 <= nSlaves
  , KnownNat addrW
  , (CLog 2 nSlaves <= addrW)
  , BitPack a
  , NFDataX a
  ) =>
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
    oneHotOrZeroSelected = fmap (== addrIndex) config
    (addrIndex, newAddr) =
      split @_ @_ @(MappedBusAddrWidth addrW nSlaves) addr
    toSlaves =
      (\newStrobe -> (updateM2SAddr newAddr master){strobe = strobe && newStrobe})
        <$> oneHotOrZeroSelected
    toMaster
      | busCycle && strobe =
          foldMaybes
            emptyWishboneS2M{err = True} -- master tries to access unmapped memory
            (maskToMaybes slaves oneHotOrZeroSelected)
      | otherwise = emptyWishboneS2M

dupWb ::
  forall dom aw.
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

type WbToBool dom mode addrW a =
  Fwd (Wishbone dom mode addrW a) ->
  Bwd (Wishbone dom mode addrW a) ->
  Signal dom Bool

-- | busCycle && strobe
onRequestWb :: WbToBool dom mode addrW a
onRequestWb = liftA2 $ \m _ -> m.busCycle && m.strobe

-- | busCycle && strobe && (acknowledge || err || stall || retry)
onTransactionWb :: forall dom mode addrW a. WbToBool dom mode addrW a
onTransactionWb = liftA2 $ \m s -> m.busCycle && m.strobe && (s.acknowledge || s.err || s.stall || s.retry)

-- | busCycle && strobe && addr >= lower && addr < upper
inAddrRangeWb ::
  forall dom mode addrW a.
  (KnownNat addrW) =>
  BitVector addrW ->
  BitVector addrW ->
  WbToBool dom mode addrW a
inAddrRangeWb lower upper = liftA2 (\m _ -> m.busCycle && m.strobe && m.addr >= lower && m.addr < upper)

{- | An ILA monitoring all M2S and S2M signals on a Wishbone bus. Installs two
extra signals 'capture' and 'trigger' that can be used as defaults for triggering
the ILA and conditional capturing. Trigger will be active for every valid
transaction, while capture will be active for as long as trigger and a cycle
after it.
-}
ilaWb ::
  forall name dom addrW a.
  (HiddenClock dom) =>
  -- | Name of the module of the `ila` wrapper. Naming the internal ILA is
  -- unreliable when more than one ILA is used with the same arguments, but the
  -- module name can be set reliably.
  SSymbol name ->
  -- | Number of registers to insert at each probe. Supported values: 0-6.
  -- Corresponds to @C_INPUT_PIPE_STAGES@. Default is @0@.
  Index 7 ->
  -- | Number of samples to store. Corresponds to @C_DATA_DEPTH@. Default set
  -- by 'ilaConfig' equals 'D4096'.
  Depth ->
  WbToBool dom 'Standard addrW a ->
  WbToBool dom 'Standard addrW a ->
  Circuit
    (Wishbone dom 'Standard addrW a)
    (Wishbone dom 'Standard addrW a)
ilaWb SSymbol stages0 depth0 trigger capture = Circuit $ \(m2s, s2m) ->
  let
    -- Our HITL test infrastructure looks for 'trigger' and 'capture' and uses
    -- it to trigger the ILA and do selective capture. Though defaults are
    -- changable using Vivado, we set it to capture only valid Wishbone
    -- transactions plus a single cycle after it.

    ilaInst :: Signal dom ()
    ilaInst =
      setName @name
        $ ila
          ( ( ilaConfig
                $ "m2s_addr"
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
                :> Nil
            )
              { advancedTriggers = True
              , stages = stages0
              , depth = depth0
              }
          )
          hasClock
          (Wishbone.addr <$> m2s)
          (Wishbone.writeData <$> m2s)
          (Wishbone.busSelect <$> m2s)
          (Wishbone.busCycle <$> m2s)
          (Wishbone.strobe <$> m2s)
          (Wishbone.writeEnable <$> m2s)
          (Wishbone.readData <$> s2m)
          (Wishbone.acknowledge <$> s2m)
          (Wishbone.err <$> s2m)
          (Wishbone.stall <$> s2m)
          (Wishbone.retry <$> s2m)
          (capture m2s s2m)
          (trigger m2s s2m)
   in
    ilaInst `hwSeqX` (s2m, m2s)

{- | Given a vector with elements and a mask, promote all values with a corresponding
'True' to 'Just', others to 'Nothing'.

Example:

>>> maskToMaybes ('a' :> 'b' :> Nil) (True :> False :> Nil)
Just 'a' :> Nothing :> Nil
-}
maskToMaybes :: Vec n a -> Vec n Bool -> Vec n (Maybe a)
maskToMaybes = zipWith (bool Nothing . Just)

{- | Fold 'Maybe's to a single value. If the given vector does not contain any 'Just',
the default value is picked. Prefers the leftmost value when the vector contains
multiple 'Just's.

Example:

>>> foldMaybes 'a' (Nothing :> Just 'c' :> Nil)
'c'
>>> foldMaybes 'a' (Just 'b' :> Just 'c' :> Nil)
'b'
>>> foldMaybes 'a' (Nothing :> Nothing :> Nil)
'a'
-}
foldMaybes :: a -> Vec n (Maybe a) -> a
foldMaybes a Nil = a
foldMaybes dflt v@(Cons _ _) = fromMaybe dflt $ fold (<|>) v

{- | Version of 'singleMasterInterconnect' that does not use the 'Circuit' abstraction
from @clash-protocols@ but exposes 'Signal's directly.
-}
singleMasterInterconnect' ::
  forall dom nSlaves addrW a.
  ( HiddenClockResetEnable dom
  , KnownNat nSlaves
  , 1 <= nSlaves
  , KnownNat addrW
  , CLog 2 nSlaves <= addrW
  , BitPack a
  , NFDataX a
  ) =>
  MemoryMap nSlaves ->
  Signal dom (WishboneM2S addrW (Regs a 8) a) ->
  Signal dom (Vec nSlaves (WishboneS2M a)) ->
  ( Signal dom (WishboneS2M a)
  , Signal dom (Vec nSlaves (WishboneM2S (MappedBusAddrWidth addrW nSlaves) (Regs a 8) a))
  )
singleMasterInterconnect' config master slaves = (toMaster, bundle toSlaves)
 where
  Circuit f = singleMasterInterconnect @dom @nSlaves @addrW @a config
  (toMaster, toSlaves) =
    case divWithRemainder @(Regs a 8) @8 @7 of
      Dict ->
        f (master, unbundle slaves)

{- | Takes an input that features no back pressure mechanism and turn it into `Df`.
This function is unsafe because data can be lost when the input is @Just _@ and
the receiving circuit tries to apply back pressure.
-}
unsafeToDf :: Circuit (CSignal dom (Maybe a)) (Df dom a)
unsafeToDf = Circuit $ \(cSig, _) -> (pure (), Df.maybeToData <$> cSig)

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
  go ((request, rxBit), _) =
    ( (Ack <$> ack, pure ())
    , (received, txBit)
    )
   where
    (received, txBit, ack) = uart baud rxBit (Df.dataToMaybe <$> request)

-- | Component compatible with `uartInterfaceWb` for simulation purposes.
uartSim ::
  (HiddenClockResetEnable dom) =>
  -- | Left side of circuit: word to send, receive interface
  -- Right side of circuit: received word, transmit interface
  Circuit
    ( Df dom (BitVector 8)
    , Df dom (BitVector 8)
    )
    ( CSignal dom (Maybe (BitVector 8))
    , Df dom (BitVector 8)
    )
uartSim = Circuit go
 where
  go ((txByte, rxByte), (_, ack)) =
    ((ack, pure $ Ack True), (Df.dataToMaybe <$> rxByte, txByte))

{- | Wishbone accessible UART interface with configurable FIFO buffers.
  It takes the depths of the transmit and receive buffers and the uart implementation
  as parameters. By explicitly passing the uart implementation, the user can choose
  to either use a 'uartDf' circuit for actual serial communication or use `uartSim`
  for simulation purposes. Alongside the uart interface, the component produces
  a 'CSignal' tuple indicating the status of the transmit and receive buffers.

  The register layout is as follows:
  - Address 0 (BitVector 8): UART data register (read/write)
  - Address 4 (BitVector 2): UART status register (read-only)
    Relevant masks:
    - 0b01: Transmit buffer full
    - 0b10: Receive buffer empty
-}
uartInterfaceWb ::
  forall dom addrW nBytes transmitBufferDepth receiveBufferDepth uartIn uartOut.
  ( HiddenClockResetEnable dom
  , 1 <= transmitBufferDepth
  , 1 <= receiveBufferDepth
  , KnownNat addrW
  , KnownNat nBytes
  , 1 <= nBytes
  ) =>
  -- | Recommended value: 16. This seems to be a good balance between resource
  -- usage and usability.
  SNat transmitBufferDepth ->
  -- | Recommended value: 16. This seems to be a good balance between resource
  -- usage and usability.
  SNat receiveBufferDepth ->
  -- | Valid baud rates are constrained by @clash-cores@'s 'ValidBaud' constraint.
  Circuit (Df dom (BitVector 8), uartIn) (CSignal dom (Maybe (BitVector 8)), uartOut) ->
  Circuit
    (Wishbone dom 'Standard addrW (Bytes nBytes), uartIn)
    (uartOut, CSignal dom (Bool, Bool))
uartInterfaceWb txDepth@SNat rxDepth@SNat uartImpl = circuit $ \(wb, uartRx) -> do
  (txFifoIn, uartStatus) <- wbToDf -< (wb, rxFifoOut, txFifoMeta)
  (txFifoOut, txFifoMeta) <- fifoWithMeta txDepth -< txFifoIn
  (rxFifoIn, uartTx) <- uartImpl -< (txFifoOut, uartRx)
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
  wbToDf =
    Circuit
      $ bimap unbundle unbundle
      . unbundle
      . fmap go
      . bundle
      . bimap bundle bundle
   where
    go ((WishboneM2S{..}, Df.dataToMaybe -> rxData, fifoFull -> txFull), (Ack txAck, _))
      -- not in cycle
      | not (busCycle && strobe) =
          ( ((emptyWishboneS2M @()){readData = invalidReq}, Ack False, ())
          , (Df.NoData, status)
          )
      -- illegal addr
      | not addrLegal =
          ( ((emptyWishboneS2M @()){err = True, readData = invalidReq}, Ack False, ())
          , (Df.NoData, status)
          )
      -- read at 0
      | not writeEnable && internalAddr == 0 =
          (
            ( (emptyWishboneS2M @())
                { acknowledge = True
                , readData = resize $ fromMaybe 0 rxData
                }
            , Ack True
            , ()
            )
          , (Df.NoData, status)
          )
      -- write at 0
      | writeEnable && internalAddr == 0 =
          (
            ( (emptyWishboneS2M @())
                { acknowledge = txAck
                , readData = invalidReq
                }
            , Ack False
            , ()
            )
          , (Df.Data $ resize writeData, status)
          )
      -- read at 1
      | not writeEnable && internalAddr == 1 =
          (
            ( (emptyWishboneS2M @())
                { acknowledge = True
                , readData = resize $ pack status
                }
            , Ack False
            , ()
            )
          , (Df.NoData, status)
          )
      | otherwise = ((emptyWishboneS2M{err = True}, Ack False, ()), (Df.NoData, status))
     where
      internalAddr = bitCoerce $ resize addr :: Index 2
      addrLegal = addr <= 1
      rxEmpty = isNothing rxData
      status = (rxEmpty, txFull)
      invalidReq =
        deepErrorX
          [i|uartInterfaceWb: Invalid request.
          BUS: {busCycle}
          STR: {strobe}
          ADDR: {addr}
          WE:{writeEnable}
          ACK:{acknowledge}
          ERR:{err}|]

-- | State record for the FIFO circuit.
data FifoState depth = FifoState
  { readPointer :: Index depth
  , dataCount :: Index (depth + 1)
  }
  deriving (Generic, NFDataX)

-- | Meta information from 'fifoWithMeta'.
data FifoMeta depth = FifoMeta
  { fifoEmpty :: Bool
  , fifoFull :: Bool
  , fifoDataCount :: Index (depth + 1)
  }
  deriving (Generic, NFDataX)

{- | A generic First-In-First-Out (FIFO) circuit with a specified depth that exposes
meta information such as in `FifoMeta`. At least one cycle latency.
When the reset is high or the enable is low, there will be no outgoing transactions and
incoming transactions are not acknowledged.
-}
fifoWithMeta ::
  forall dom a depth.
  (HiddenClockResetEnable dom, 1 <= depth, NFDataX a) =>
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
      readNew
        (blockRamU NoClearOnReset depth (errorX "No reset function"))
        readAddr
        writeOp
    (readAddr, writeOp, fifoOut, readyOut, fifoMeta) =
      mealyB go initialState (circuitActive, fifoIn, readyIn, bramOut)

  -- Initial state of the FIFO
  initialState =
    FifoState
      { readPointer = 0
      , dataCount = 0
      }
  go ::
    FifoState depth ->
    (Bool, Df.Data a, Ack, a) ->
    ( FifoState depth
    , (Index depth, Maybe (Index depth, a), Df.Data a, Bool, FifoMeta depth)
    )
  go state@FifoState{..} (False, _, _, _) = (state, (readPointer, Nothing, Df.NoData, False, fifoMeta))
   where
    fifoEmpty = dataCount == 0
    fifoFull = dataCount == maxBound
    fifoMeta = FifoMeta{fifoEmpty, fifoFull, fifoDataCount = dataCount}
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
      _ -> id

    nextState =
      FifoState
        { readPointer = readPointerNext
        , dataCount = dataCountNext
        }

    fifoMeta = FifoMeta{fifoEmpty, fifoFull, fifoDataCount = dataCount}
    output = (readPointerNext, writeOpGo, fifoOutGo, not fifoFull, fifoMeta)

{- | Transforms a wishbone interface into a vector based interface.
Write operations will produce a 'Just (Bytes nBytes)' on the index corresponding
to the word-aligned Wishbone address.
Read operations will read from the index corresponding to the world-aligned
Wishbone address.
-}
wbToVec ::
  forall nBytes addrW nRegisters.
  ( KnownNat nBytes
  , 1 <= nBytes
  , KnownNat addrW
  , KnownNat nRegisters
  , 1 <= nRegisters
  ) =>
  -- | Readable data.
  Vec nRegisters (Bytes nBytes) ->
  -- | Wishbone bus (master to slave)
  WishboneM2S addrW nBytes (Bytes nBytes) ->
  -- |
  -- 1. Written data
  -- 2. Outgoing wishbone bus (slave to master)
  ( Vec nRegisters (Maybe (Bytes nBytes))
  , WishboneS2M (Bytes nBytes)
  )
wbToVec readableData WishboneM2S{..} = (writtenData, wbS2M)
 where
  masterActive = strobe && busCycle
  err = masterActive && (addr > resize (pack (maxBound :: Index nRegisters)))
  acknowledge = masterActive && not err
  wbWriting = writeEnable && acknowledge
  wbAddr = unpack $ resize addr :: Index nRegisters
  readData = readableData !! wbAddr
  writtenData
    | wbWriting = replace wbAddr (Just writeData) (repeat Nothing)
    | otherwise = repeat Nothing
  wbS2M = (emptyWishboneS2M @(Bytes 4)){acknowledge, readData, err}

{- | Wishbone accessible circuit that contains a free running 64 bit counter. We can
observe this counter to get a sense of time, overflows should be accounted for by
the master.
-}
timeWb ::
  forall dom addrW.
  ( HiddenClockResetEnable dom
  , KnownNat addrW
  , 1 <= DomainPeriod dom
  ) =>
  Circuit (Wishbone dom 'Standard addrW (Bytes 4)) ()
timeWb = Circuit $ \(wbM2S, _) -> (mealy goMealy (0, 0) wbM2S, ())
 where
  goMealy (frozen, count :: Unsigned 64) wbM2S = ((nextFrozen, succ count), wbS2M)
   where
    freq = natToNum @(DomainToHz dom) :: Unsigned 64
    nextFrozen = if isJust (head writes) then count else frozen
    RegisterBank (splitAtI -> (frozenMsbs, frozenLsbs)) = getRegsBe @8 frozen
    RegisterBank (splitAtI -> (freqMsbs, freqLsbs)) = getRegsBe @8 freq
    (writes, wbS2M) =
      wbToVec
        (0 :> fmap pack (frozenLsbs :> frozenMsbs :> freqLsbs :> freqMsbs :> Nil))
        wbM2S

{- | Wishbone wrapper for DnaPortE2, adds extra register with wishbone interface
to access the DNA device identifier. The DNA device identifier is a 96-bit
value, stored in big-endian format.
-}
readDnaPortE2Wb ::
  forall dom addrW nBytes.
  ( HiddenClockResetEnable dom
  , KnownNat addrW
  , KnownNat nBytes
  , 1 <= nBytes
  ) =>
  -- | Simulation DNA value
  BitVector 96 ->
  Circuit (Wishbone dom 'Standard addrW (Bytes nBytes)) ()
readDnaPortE2Wb simDna = circuit $ \wb -> do
  dnaDf <- dnaCircuit -< ()
  _dna <- reg -< (wb, dnaDf)
  idC -< ()
 where
  maybeDna = readDnaPortE2 hasClock hasReset hasEnable simDna
  regRst =
    unsafeFromActiveHigh
      $ register True
      $ fmap isNothing maybeDna
      .||. unsafeToActiveHigh hasReset
  reg = withReset regRst $ registerWbC @dom @_ @nBytes @addrW WishbonePriority 0
  dnaCircuit :: Circuit () (Df dom (BitVector 96))
  dnaCircuit = Circuit $ const ((), Df.maybeToData <$> maybeDna)
