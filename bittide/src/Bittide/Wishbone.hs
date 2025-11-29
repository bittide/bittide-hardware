-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

module Bittide.Wishbone where

-- prelude imports
import Clash.Prelude hiding (Exp)

-- external imports
import Clash.Class.BitPackC
import Clash.Cores.UART (ValidBaud, uart)
import Clash.Cores.Xilinx.Ila (Depth, IlaConfig (..), ila, ilaConfig)
import Clash.Cores.Xilinx.Unisim.DnaPortE2
import Clash.Debug
import Clash.Functor.Extra ((<<$>>))
import Clash.Util.Interpolate
import Control.DeepSeq (NFData)
import Data.Bool (bool)
import Data.Constraint.Nat.Extra
import Data.Constraint.Nat.Lemmas
import Data.Maybe
import GHC.Stack (HasCallStack)
import Protocols
import Protocols.Idle (forceResetSanityGeneric)
import Protocols.MemoryMap.Registers.WishboneStandard (
  RegisterConfig (access),
  registerConfig,
  registerWbI_,
 )
import Protocols.MemoryMap.TypeDescription.TH
import Protocols.Wishbone

-- internal imports
import Bittide.Df hiding (wbToDf)
import Bittide.Extra.Maybe
import Bittide.SharedTypes

-- qualified imports

import qualified Data.List as L
import qualified Protocols.MemoryMap as MM
import qualified Protocols.MemoryMap.Registers.WishboneStandard as MM
import qualified Protocols.Vec as Vec
import qualified Protocols.Wishbone as Wishbone

{- $setup
>>> import Clash.Prelude
-}

-- Applying this hint yields a compile error
{-# ANN module "HLint: ignore Functor law" #-}

-- | A vector of base addresses, one for each slave.
type MemoryMap nSlaves pfxWidth = Vec nSlaves (Unsigned pfxWidth)

singleMasterInterconnectC ::
  forall dom nSlaves addrW pfxWidth a.
  ( HiddenClockResetEnable dom
  , HasCallStack
  , KnownNat nSlaves
  , 1 <= nSlaves
  , KnownNat addrW
  , KnownNat pfxWidth
  , (pfxWidth <= addrW)
  , BitPack a
  , NFDataX a
  ) =>
  Circuit
    (MM.ConstBwd MM.MM, Wishbone dom 'Standard addrW a)
    ( Vec
        nSlaves
        ( MM.ConstBwd (Unsigned pfxWidth)
        , (MM.ConstBwd MM.MM, Wishbone dom 'Standard (addrW - pfxWidth) a)
        )
    )
singleMasterInterconnectC = Circuit go
 where
  go ::
    ( ((), Signal dom (WishboneM2S addrW (Div (BitSize a + 7) 8) a))
    , Vec
        nSlaves
        (Unsigned pfxWidth, (SimOnly MM.MemoryMap, Signal dom (WishboneS2M a)))
    ) ->
    ( (SimOnly MM.MemoryMap, Signal dom (WishboneS2M a))
    , Vec
        nSlaves
        ((), ((), Signal dom (WishboneM2S (addrW - pfxWidth) (Div (BitSize a + 7) 8) a)))
    )
  go (((), m2s), unzip -> (prefixes, unzip -> (slaveMms, s2ms))) = ((SimOnly memMap, s2m), (\x -> ((), ((), x))) <$> m2ss)
   where
    -- the 4 * is needed because the addrW etc relies on a word-aligned bus,
    -- not a byte aligned bus.
    prefixToAddr prefix = 4 * (toInteger prefix `shiftL` fromInteger shift')
     where
      shift' = snatToInteger $ SNat @(addrW - pfxWidth)
    relAddrs = L.map prefixToAddr (toList prefixes)
    comps = L.zip relAddrs ((.tree) . unSimOnly <$> toList slaveMms)
    unSimOnly (SimOnly n) = n
    deviceDefs = MM.mergeDeviceDefs ((.deviceDefs) . unSimOnly <$> toList slaveMms)
    memMap =
      MM.MemoryMap
        { tree = MM.Interconnect MM.locCaller comps
        , deviceDefs = deviceDefs
        }
    (s2m, m2ss) = toSignals (singleMasterInterconnect prefixes) (m2s, s2ms)

{-# OPAQUE singleMasterInterconnect #-}

{- | Component that maps multiple slave devices to a single master device over the wishbone
bus. It routes the incoming control signals to a slave device based on the 'MemoryMap',
a vector of base addresses.
-}
singleMasterInterconnect ::
  forall dom nSlaves addrW pfxWidth a.
  ( HiddenClockResetEnable dom
  , KnownNat nSlaves
  , 1 <= nSlaves
  , KnownNat addrW
  , KnownNat pfxWidth
  , pfxWidth <= addrW
  , BitPack a
  , NFDataX a
  ) =>
  MemoryMap nSlaves pfxWidth ->
  Circuit
    (Wishbone dom 'Standard addrW a)
    (Vec nSlaves (Wishbone dom 'Standard (addrW - pfxWidth) a))
singleMasterInterconnect (fmap pack -> config) =
  Circuit go
 where
  go (masterS, slavesS) =
    fmap unbundle . unbundle $ route <$> masterS <*> bundle slavesS

  route master@(WishboneM2S{..}) slaves =
    ( strictV slaves `seqX` strictV toSlaves `seqX` toMaster
    , toSlaves
    )
   where
    oneHotOrZeroSelected = fmap (== addrIndex) config
    (addrIndex :: BitVector pfxWidth, newAddr) = split addr
    toSlaves =
      (\newStrobe -> (updateM2SAddr newAddr master){strobe = strobe && newStrobe})
        <$> oneHotOrZeroSelected
    toMaster
      | busCycle && strobe =
          foldMaybes
            emptyWishboneS2M{err = True} -- master tries to access unmapped memory
            (maskToMaybes slaves oneHotOrZeroSelected)
      | otherwise = emptyWishboneS2M

  strictV :: Vec m b -> Vec m b
  strictV v
    | clashSimulation = foldl (\b a -> a `seqX` b) () v `seqX` v
    | otherwise = v

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

{- | Conditionally sequences the 'ilaWb' function based on whether the input 'Bool' is
'True' or 'False'.
-}
maybeIlaWb ::
  forall name dom addrW a.
  (HiddenClock dom) =>
  -- | Whether or not this ILA instance should be real or not. 'True' actually creates
  -- the ILA, 'False' makes this circuit element a no-op.
  Bool ->
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
maybeIlaWb True a b c d e = ilaWb a b c d e
maybeIlaWb False _ _ _ _ _ = circuit $ \left -> do
  idC -< left

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
  forall dom nSlaves addrW pfxWidth a.
  ( HiddenClockResetEnable dom
  , KnownNat nSlaves
  , 1 <= nSlaves
  , KnownNat addrW
  , KnownNat pfxWidth
  , pfxWidth <= addrW
  , BitPack a
  , NFDataX a
  ) =>
  MemoryMap nSlaves pfxWidth ->
  Signal dom (WishboneM2S addrW (Regs a 8) a) ->
  Signal dom (Vec nSlaves (WishboneS2M a)) ->
  ( Signal dom (WishboneS2M a)
  , Signal dom (Vec nSlaves (WishboneM2S (addrW - pfxWidth) (Regs a 8) a))
  )
singleMasterInterconnect' config master slaves = (toMaster, bundle toSlaves)
 where
  Circuit f = singleMasterInterconnect @dom @nSlaves @addrW @pfxWidth @a config
  (toMaster, toSlaves) =
    case divWithRemainder @(Regs a 8) @8 @7 of
      Dict ->
        f (master, unbundle slaves)

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
    ( (Ack <$> ack, ())
    , (received, txBit)
    )
   where
    (received, txBit, ack) = uart baud rxBit request

-- | Component compatible with `uartInterfaceWb` for simulation purposes.
uartBytes ::
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
uartBytes = Circuit go
 where
  go ((txByte, rxByte), (_, ack)) =
    ((ack, pure $ Ack True), (rxByte, txByte))

{- | Wishbone accessible UART interface with configurable FIFO buffers.
  It takes the depths of the transmit and receive buffers and the uart implementation
  as parameters. By explicitly passing the uart implementation, the user can choose
  to either use a 'uartDf' circuit for actual serial communication or use `uartBytes`
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
  , HasCallStack
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
    ((MM.ConstBwd MM.MM, Wishbone dom 'Standard addrW (Bytes nBytes)), uartIn)
    (uartOut, CSignal dom (Bool, Bool))
uartInterfaceWb txDepth@SNat rxDepth@SNat uartImpl = circuit $ \((mm, wb), uartRx) -> do
  (txFifoIn, uartStatus) <- wbToDf -< (wb, rxFifoOut, txFifoMeta)
  (txFifoOut, txFifoMeta) <- fifoWithMeta txDepth -< txFifoIn
  (rxFifoIn, uartTx) <- uartImpl -< (txFifoOut, uartRx)
  (rxFifoOut, _rx') <- fifoWithMeta rxDepth <| unsafeToDf -< rxFifoIn
  MM.constBwd memMap -< mm
  idC -< (uartTx, uartStatus)
 where
  memMap =
    SimOnly
      $ MM.MemoryMap
        { tree = MM.DeviceInstance MM.locCaller "Uart"
        , deviceDefs = MM.deviceSingleton deviceDef
        }

  deviceDef =
    MM.DeviceDefinition
      { registers =
          [ MM.NamedLoc
              { name = MM.Name "data" ""
              , loc = MM.locHere
              , value =
                  MM.Register
                    { reset = Nothing
                    , fieldType = MM.regType @(Bytes 1)
                    , address = 0
                    , access = MM.ReadWrite
                    , tags = []
                    }
              }
          , MM.NamedLoc
              { name = MM.Name "status" ""
              , loc = MM.locHere
              , value =
                  MM.Register
                    { reset = Nothing
                    , fieldType = MM.regType @(Bytes 1)
                    , address = 4
                    , access = MM.ReadOnly
                    , tags = []
                    }
              }
          ]
      , deviceName =
          MM.Name "Uart" "Wishbone accessible UART interface with configurable FIFO buffers."
      , definitionLoc = MM.locHere
      , tags = []
      }

  wbToDf ::
    Circuit
      ( Wishbone dom 'Standard addrW (Bytes nBytes)
      , Df dom (BitVector 8)
      , CSignal dom (FifoMeta txFifoDepth)
      )
      ( Df dom (BitVector 8)
      , CSignal dom (Bool, Bool) -- (rxEmpty, txFull)
      )
  wbToDf = Circuit go0
   where
    go0 ((m2s, dfDataIn, fifoMeta), (ackIn, _)) =
      ((s2m, ack, ()), (dfOut, status))
     where
      (s2m, ack, dfOut, status) = unbundle (fmap go1 (bundle (m2s, dfDataIn, fifoMeta, ackIn)))

    go1 (WishboneM2S{..}, rxData, (.fifoFull) -> txFull, Ack txAck)
      -- not in cycle
      | not (busCycle && strobe) =
          ( (emptyWishboneS2M @()){readData = invalidReq}
          , Ack False
          , Nothing
          , status
          )
      -- illegal addr
      | not addrLegal =
          ( (emptyWishboneS2M @()){err = True, readData = invalidReq}
          , Ack False
          , Nothing
          , status
          )
      -- read at 0
      | not writeEnable && internalAddr == 0 =
          ( (emptyWishboneS2M @())
              { acknowledge = True
              , readData = resize $ fromMaybe 0 rxData
              }
          , Ack True
          , Nothing
          , status
          )
      -- write at 0
      | writeEnable && internalAddr == 0 =
          ( (emptyWishboneS2M @())
              { acknowledge = txAck
              , readData = invalidReq
              }
          , Ack False
          , Just $ resize writeData
          , status
          )
      -- read at 1
      | not writeEnable && internalAddr == 1 =
          ( (emptyWishboneS2M @())
              { acknowledge = True
              , readData = resize $ pack status
              }
          , Ack False
          , Nothing
          , status
          )
      | otherwise = (emptyWishboneS2M{err = True}, Ack False, Nothing, status)
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
        (blockRamU NoClearOnReset depth)
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
    (Bool, Maybe a, Ack, a) ->
    ( FifoState depth
    , (Index depth, Maybe (Index depth, a), Maybe a, Bool, FifoMeta depth)
    )
  go state@FifoState{..} (False, _, _, _) = (state, (readPointer, Nothing, Nothing, False, fifoMeta))
   where
    fifoEmpty = dataCount == 0
    fifoFull = dataCount == maxBound
    fifoMeta = FifoMeta{fifoEmpty, fifoFull, fifoDataCount = dataCount}
  go FifoState{..} (True, fifoIn, Ack readyIn, bramOut) = (nextState, output)
   where
    fifoEmpty = dataCount == 0
    fifoFull = dataCount == maxBound
    writePointer = satAdd SatWrap readPointer $ resize dataCount

    readSuccess = not fifoEmpty && readyIn
    writeSuccess = not fifoFull && isJust fifoIn

    readPointerNext = if readSuccess then satSucc SatWrap readPointer else readPointer
    writeOpGo = if writeSuccess then (writePointer,) <$> fifoIn else Nothing
    fifoOutGo = if fifoEmpty then Nothing else Just bramOut

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

data TimeCmd = Capture | WaitForCmp
  deriving (Eq, Generic, Show, NFDataX, BitPack, BitPackC)
deriveTypeDescription ''TimeCmd

{- FOURMOLU_DISABLE -}
{- | Wishbone accessible circuit that contains a free running 64 bit counter with stalling
capabilities.
-}
timeWb ::
  forall dom addrW.
  ( HiddenClockResetEnable dom
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  , HasCallStack
  , KnownNat addrW
  , 1 <= DomainPeriod dom
  ) =>
  Circuit
    (MM.ConstBwd MM.MM, Wishbone dom 'Standard addrW (Bytes 4))
    (CSignal dom (Unsigned 64))
timeWb = circuit $ \mmWb -> do
  [(cmdOffset, cmdMeta, cmdWb0), cmpWb, scratchWb, freqWb] <- MM.deviceWb "Timer" -< mmWb
  cmdWb1 <- andAck cmdWaitAck -< cmdWb0
  cmdWb2 <- idC -< (cmdOffset, cmdMeta, cmdWb1)

  -- Registers
  Fwd (_, cmdActivity) <- MM.registerWbI cmdCfg Capture -< (cmdWb2, Fwd noWrite)
  MM.registerWbI_ cmpCfg False -< (cmpWb, Fwd cmpResultWrite)
  Fwd (scratch, _) <- MM.registerWbI scratchCfg (0 :: Unsigned 64) -< (scratchWb, Fwd scratchWrite)
  MM.registerWbI_ freqCfg freq -< (freqWb, Fwd noWrite)

  -- Local circuit dependent declarations
  let
    scratchWrite = orNothing <$> (cmdActivity .== Just (MM.BusWrite Capture)) <*> count
    cmpResult = count .>=. scratch
    cmpResultWrite = orNothing <$> (cmdActivity ./= Just (MM.BusWrite WaitForCmp)) <*> cmpResult
    cmdWaitAck = (cmdActivity ./= Just (MM.BusWrite WaitForCmp)) .||. cmpResult
  idC -< Fwd count
 where
  -- Independent declarations
  freq = natToNum @(DomainToHz dom) :: Unsigned 64
  noWrite = pure Nothing
  count = register 0 (count + 1)

  -- Register configurations
  cmdCfg =
    (MM.registerConfig "command")
      { MM.access = MM.WriteOnly
      , MM.description = "Control register"
      }
  cmpCfg =
    (MM.registerConfig "cmp_result")
      { MM.access = MM.ReadOnly
      , MM.description = "Comparison result"
      }
  scratchCfg =
    (MM.registerConfig "scratchpad")
      { MM.access = MM.ReadWrite
      , MM.description = "Scratch pad"
      }
  freqCfg =
    (MM.registerConfig "frequency")
      { MM.access = MM.ReadOnly
      , MM.description = "Frequency of the clock domain"
      }
{- FOURMOLU_ENABLE -}

andAck ::
  Signal dom Bool ->
  Circuit
    (Wishbone dom 'Standard addrW (Bytes nBytes))
    (Wishbone dom 'Standard addrW (Bytes nBytes))
andAck extraAck = Circuit go
 where
  go (m2s, s2m0) = (s2m1, m2s)
   where
    s2m1 = (\wb ack -> wb{acknowledge = wb.acknowledge && ack}) <$> s2m0 <*> extraAck

{- | Multi-manager, single subordinate interconnect. It is currently not configurable
in its priority which means managers might starve. It will always prefer the
manager with the lowest index.

XXX: This arbiter does not support @LOCK@ cycles.
-}
arbiter ::
  forall dom addrW a n.
  ( HiddenClockResetEnable dom
  , KnownNat addrW
  , BitPack a
  , NFDataX a
  , KnownNat n
  ) =>
  Circuit
    (Vec n (Wishbone dom 'Standard addrW a))
    (Wishbone dom 'Standard addrW a)
arbiter = Circuit goArbitrate0
 where
  -- Bundler / unbundler for 'goArbitrate1'
  goArbitrate0 (bundle -> m2ss, s2m) = (unbundle s2ms, m2s)
   where
    (s2ms, m2s) = mealyB goArbitrate1 (Nothing @(Index n)) (m2ss, s2m)

  -- Actual worker
  goArbitrate1 current (m2ss, s2m) = (next, (s2ms, m2s))
   where
    candidate = findIndex (.busCycle) m2ss
    selected = maybe candidate Just current
    m2s = maybe emptyWishboneM2S (m2ss !!) selected

    -- Always route the read data from the subordinate to all managers to prevent
    -- muxing. Managers will only look at the read data when they get an
    -- acknowledgement.
    emptyS2Ms = repeat (emptyWishboneS2M @a){readData = s2m.readData}
    s2ms = case selected of
      Nothing -> emptyS2Ms
      Just idx -> replace idx s2m emptyS2Ms

    next =
      case selected of
        Just idx | not s2m.acknowledge -> Just idx
        _ -> candidate

-- | Like 'arbiter', but also handles memory maps.
arbiterMm ::
  forall dom addrW a n.
  ( HiddenClockResetEnable dom
  , KnownNat addrW
  , BitPack a
  , NFDataX a
  , KnownNat n
  ) =>
  Circuit
    ( Vec n (MM.ConstBwd MM.MM, Wishbone dom 'Standard addrW a)
    )
    ( MM.ConstBwd MM.MM
    , Wishbone dom 'Standard addrW a
    )
arbiterMm = circuit $ \mmWbs -> do
  (mms, wbs) <- Vec.unzip -< mmWbs
  mm <- Circuit goDuplicate -< mms
  wb <- arbiter -< wbs
  idC -< (mm, wb)
 where
  goDuplicate (_, mm) = (repeat mm, ())

{- | Wishbone wrapper for DnaPortE2, adds extra register with wishbone interface
to access the DNA device identifier. The DNA device identifier is a 96-bit
value.
-}
readDnaPortE2Wb ::
  forall dom addrW nBytes.
  ( HiddenClockResetEnable dom
  , HasCallStack
  , KnownNat addrW
  , KnownNat nBytes
  , 1 <= nBytes
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | Simulation DNA value
  BitVector 96 ->
  Circuit
    (MM.ConstBwd MM.MM, Wishbone dom 'Standard addrW (Bytes nBytes))
    (CSignal dom (BitVector 96))
readDnaPortE2Wb simDna = circuit $ \wb -> do
  [maybeDnaWb] <- MM.deviceWb "Dna" -< wb
  registerWbI_ config Nothing -< (maybeDnaWb, Fwd (Just <<$>> maybeDna))
  -- XXX: It's slightly iffy to use fromMaybe here, but in practice nothing will
  --      use it until the DNA is actually read out.
  idC -< Fwd (fromMaybe 0 <$> maybeDna)
 where
  maybeDna = readDnaPortE2 hasClock hasReset hasEnable simDna
  config = (registerConfig "maybe_dna"){access = MM.ReadOnly}

{- | A Wishbone worker circuit that exposes the DNA value from an external DnaPortE2.
Only one DnaPortE2 can be instantiated in a design, so this component takes in the
DNA value from a DnaPortE2. It exposes the DNA value as a 'CSignal' and adds a
Wishbone register to read it out.
-}
readDnaPortE2WbWorker ::
  forall dom addrW nBytes.
  ( HiddenClockResetEnable dom
  , HasCallStack
  , KnownNat addrW
  , KnownNat nBytes
  , 1 <= nBytes
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  -- | DNA value
  Signal dom (Maybe (BitVector 96)) ->
  Circuit
    (MM.ConstBwd MM.MM, Wishbone dom 'Standard addrW (Bytes nBytes))
    ()
readDnaPortE2WbWorker maybeDna = circuit $ \wb -> do
  [maybeDnaWb] <- MM.deviceWb "Dna" -< wb
  registerWbI_ config Nothing -< (maybeDnaWb, Fwd (Just <<$>> maybeDna))
 where
  config = (registerConfig "maybe_dna"){access = MM.ReadOnly}

{- | Circuit that monitors the 'Wishbone' bus and terminates the transaction after a timeout.
Controls the 'err' signal of the 'WishboneS2M' signal and sets the outgoing 'WishboneM2S'
to an empty transaction for one cycle.
-}
watchDogWb ::
  forall dom addrW nBytes timeout.
  ( HiddenClockResetEnable dom
  , KnownNat addrW
  , KnownNat nBytes
  , 1 <= nBytes
  ) =>
  String ->
  SNat timeout ->
  Circuit
    (Wishbone dom 'Standard addrW (Bytes nBytes))
    (Wishbone dom 'Standard addrW (Bytes nBytes))
watchDogWb name timeout@SNat
  | snatToNatural timeout == 0 = idC
  | otherwise = Circuit $ unbundle . mealy go (0 :: Index (timeout + 1)) . bundle
 where
  go cnt0 ~(wbM2S0, wbS2M0) = (cnt1, (wbS2M1, wbM2S1))
   where
    wdTimeout = cnt0 == maxBound

    (wbS2M1, wbM2S1)
      | wdTimeout =
          ( wbS2M0{err = True, acknowledge = False, stall = False, retry = False}
          , wbM2S0{strobe = False}
          )
      | otherwise = (wbS2M0, wbM2S0)

    cnt1
      | wbS2M0.acknowledge || wbS2M0.err || wbS2M0.stall || wbS2M0.retry = 0
      | wdTimeout = trace ("watchDogWb - " <> name <> ": " <> show wbM2S0) 0
      | wbM2S0.busCycle && wbM2S0.strobe = succ cnt0
      | otherwise = 0

{- | Simple Wishbone component with a hardcoded @True@ on acknowledge which always returns
a fixed value.
-}
wbAlwaysAckWith ::
  forall nBytes addrW.
  ( KnownNat nBytes
  , 1 <= nBytes
  , KnownNat addrW
  ) =>
  Bytes nBytes ->
  WishboneM2S addrW nBytes (Bytes nBytes) ->
  WishboneS2M (Bytes nBytes)
wbAlwaysAckWith dat _ = (emptyWishboneS2M @(Bytes nBytes)){acknowledge = True, readData = dat}

-- | Simple type for wishbone requests supporting byte enables.
data WishboneRequest addrW nBytes
  = ReadRequest (BitVector addrW) (BitVector nBytes)
  | WriteRequest (BitVector addrW) (BitVector nBytes) (Vec nBytes Byte)
  deriving (Generic, NFData, NFDataX, Show, ShowX, Eq)

deriving instance
  (KnownNat nBytes, KnownNat addrW) => BitPack (WishboneRequest addrW nBytes)

-- | Simple type for succeeding and failing read and write wishbone transactions.
data WishboneResponse nBytes
  = ReadSuccess (Vec nBytes (Maybe Byte))
  | WriteSuccess
  | ReadError
  | WriteError
  deriving (Generic, NFData, NFDataX, Show, ShowX, Eq)

deriving instance (KnownNat nBytes) => BitPack (WishboneResponse nBytes)

{- | Receives a `Df` stream of `WishboneRequest` and acts as a Wishbone manager.
All responses to the master will be forwarded as a `Df` stream of `WishboneResponse`.
-}
dfWishboneMaster ::
  forall dom addrW nBytes.
  ( HiddenClockResetEnable dom
  , KnownNat addrW
  , KnownNat nBytes
  ) =>
  Circuit
    (Df dom (WishboneRequest addrW nBytes))
    ( Wishbone dom 'Standard addrW (Vec nBytes Byte)
    , Df dom (WishboneResponse nBytes)
    )
dfWishboneMaster =
  forceResetSanityGeneric |> case cancelMulDiv @nBytes @8 of
    Dict -> Circuit go0
     where
      initState = Nothing

      go0 (req, (s2m, ackIn)) = (ackOut, (m2s, resp))
       where
        (ackOut, m2s, resp) = mealyB go1 initState (req, s2m, ackIn)

      go1 state ~(reqFwd, wbS2M, Ack respBwd) = (nextState, (Ack reqBwd, wbM2S, respFwd))
       where
        emptyM2S :: WishboneM2S addrW nBytes (Vec nBytes Byte)
        emptyM2S =
          WishboneM2S
            { addr = 0
            , writeData = repeat 0
            , busCycle = False
            , strobe = False
            , writeEnable = False
            , busSelect = 0
            , lock = False
            , cycleTypeIdentifier = Classic
            , burstTypeExtension = LinearBurst
            }

        respStalled = isJust state && not respBwd

        nextState
          | respStalled = state
          | otherwise = do
              req <- reqFwd
              case req of
                ReadRequest _ sel
                  | wbS2M.acknowledge ->
                      Just $ ReadSuccess $ mux (unpack sel) (map Just wbS2M.readData) (repeat Nothing)
                WriteRequest{} | wbS2M.acknowledge -> Just WriteSuccess
                ReadRequest{} | wbS2M.err -> Just ReadError
                WriteRequest{} | wbS2M.err -> Just WriteError
                _ -> Nothing

        reqBwd = wbDone
        respFwd = state
        masterActive = wbM2S.busCycle && wbM2S.strobe
        wbDone = masterActive && hasTerminateFlag wbS2M

        wbM2S
          | respStalled = emptyM2S
          | otherwise = case reqFwd of
              Just (ReadRequest addr sel) -> emptyM2S{busCycle = True, strobe = True, addr = addr, busSelect = sel}
              Just (WriteRequest addr sel dat) ->
                emptyM2S
                  { busCycle = True
                  , strobe = True
                  , writeEnable = True
                  , addr = addr
                  , writeData = dat
                  , busSelect = sel
                  }
              Nothing -> emptyM2S
