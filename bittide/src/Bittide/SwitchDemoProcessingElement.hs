-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.SwitchDemoProcessingElement where

import Clash.Prelude

import Data.Tuple (swap)
import GHC.Stack (HasCallStack)

import Protocols
import Protocols.Wishbone

import Bittide.SharedTypes (Bytes)
import Bittide.Wishbone (wbToVec)
import Clash.Sized.Vector.ToTuple (vecToTuple)
import Protocols.MemoryMap

{- | Multiplying by 3 should always fit, though if n~1, the output type is `Index 3`
which doesn't fit the 3 we're multiplying by hence yielding an undefined. This
function works around that.
-}
zeroExtendTimesThree :: forall n. (1 <= n, KnownNat n) => Index n -> Index (n * 3)
zeroExtendTimesThree = truncateB . mul (3 :: Index 4)

-- | Simple processing element used for the Bittide switch demo.
switchDemoPe ::
  forall bufferSize dom.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , 1 <= bufferSize
  ) =>
  -- | Size of buffer in number of "tri-cycles". That is, we always store 3 64-bit words:
  -- local clock cycle counter, DNA (64 lsbs), DNA (32 msbs, zero-extended).
  SNat bufferSize ->
  -- | Local clock cycle counter
  Signal dom (Unsigned 64) ->
  -- | Incoming crossbar link
  Signal dom (BitVector 64) ->
  -- | Device DNA
  Signal dom (BitVector 96) ->
  -- | When to read from the crossbar link
  Signal dom (Unsigned 64) ->
  -- | How many tri-cycles to read from the crossbar link
  Signal dom (Index (bufferSize + 1)) ->
  -- | When to write to the crossbar link
  Signal dom (Unsigned 64) ->
  -- | How many tri-cycles to write to the crossbar link. Includes writing \"own\" data.
  Signal dom (Index (bufferSize + 1)) ->
  ( -- \| Outgoing crossbar link
    Signal dom (BitVector 64)
  , -- \| Buffer output
    Signal dom (Vec (bufferSize * 3) (BitVector 64))
  , -- \| Current state
    Signal dom (SimplePeState bufferSize)
  )
switchDemoPe SNat localCounter linkIn dna readStart readCycles writeStart writeCycles =
  (linkOut, buffer, peState)
 where
  readCyclesExtended = checkedResize . zeroExtendTimesThree <$> readCycles
  writeCyclesExtended = zeroExtendTimesThree <$> writeCycles

  localData :: Signal dom (Vec 3 (BitVector 64))
  localData = bundle ((pack <$> localCounter) :> unbundle dnaVec)
   where
    dnaVec :: Signal dom (Vec 2 (BitVector 64))
    dnaVec = reverse . bitCoerce . zeroExtend <$> dna

  linkOut = stateToLinkOutput <$> peState <*> buffer <*> localData <*> dna

  stateToLinkOutput ::
    SimplePeState bufferSize ->
    Vec (bufferSize * 3) (BitVector 64) ->
    Vec 3 (BitVector 64) ->
    BitVector 96 ->
    BitVector 64
  stateToLinkOutput state buf locData dna0 =
    case state of
      Write i
        | i <= 2 -> locData !! i
        | otherwise -> buf !! (i - 3)
      _ -> resize $ complement dna0

  -- \| The buffer stores all the incoming bittide data. For the Bittide Switch demo,
  -- each FPGA sends its DNA and local clock cycle counter, along with all received data.
  -- The last FPGA will therefore receive all DNAs and local clock cycle counters.
  buffer :: (HasCallStack) => Signal dom (Vec (bufferSize * 3) (BitVector 64))
  buffer = bundle $ regEn <$> initVec <*> enableVec <*> linkInVec
   where
    initVec = iterateI (+ 1) 0xABBA_ABBA_ABBA_0000
    linkInVec = repeat linkIn

    enableVec :: (HasCallStack) => Vec (bufferSize * 3) (Signal dom Bool)
    enableVec = unbundle $ go <$> peState
     where
      go :: (HasCallStack) => SimplePeState bufferSize -> Vec (bufferSize * 3) Bool
      go (Read x) = (== checkedResize x) <$> indicesI
      go _ = repeat False

  prevPeState = register Idle peState

  peState =
    update
      <$> localCounter
      <*> readStart
      <*> readCyclesExtended
      <*> writeStart
      <*> writeCyclesExtended
      <*> prevPeState
   where
    update ::
      -- \| Local clock cycle counter
      Unsigned 64 ->
      -- \| When to read from the crossbar link
      Unsigned 64 ->
      -- \| How many cycles to read from the crossbar link
      Index (bufferSize * 3 + 1) ->
      -- \| When to write to the crossbar link
      Unsigned 64 ->
      -- \| How many cycles to write to the crossbar link
      Index ((bufferSize + 1) * 3) ->
      SimplePeState bufferSize ->
      SimplePeState bufferSize
    update cntr rs rc ws wc state =
      case state of
        Idle -> nextState
        Read x
          | x >= rc - 1 -> nextState
          | otherwise -> Read (satSucc SatBound x)
        Write x
          | x >= wc - 1 -> nextState
          | otherwise -> Write (satSucc SatBound x)
     where
      nextState
        | cntr == ws && wc > 0 = Write 0
        | cntr == rs && rc > 0 = Read 0
        | otherwise = Idle

data SimplePeState bufferSize
  = Idle
  | Read (Index (bufferSize * 3 + 1))
  | Write (Index ((bufferSize + 1) * 3))
  deriving (Generic, NFDataX, Eq, Show, BitPack)

{- | Wishbone circuit wrapper for `switchDemoPe`.

Buffer uses 64-bit words internally, but WB interface is 32-bit.

The register layout is as follows (lsbs in first 32-bit word, msbs in second):
- Address  0 - 1: read start
- Address  2 - 3: read cycles
- Address  4 - 5: write start
- Address  6 - 7: write cycles
- Address  8 - 9: local clock cycle counter
- Address 10 - .: buffer (bufferSize*3*2)
-}
switchDemoPeWb ::
  forall bufferSize dom addrW.
  ( HiddenClockResetEnable dom
  , KnownNat addrW
  , HasCallStack
  , 1 <= bufferSize
  ) =>
  SNat bufferSize ->
  -- | Local clock cycle counter
  Circuit
    ( ToConstBwd Mm
    , ( CSignal dom (Unsigned 64)
      , Wishbone dom 'Standard addrW (Bytes 4)
      , -- \| Device DNA
        CSignal dom (BitVector 96)
      , -- \| Incoming crossbar link
        CSignal dom (BitVector 64)
      )
    )
    ( -- \| Outgoing crossbar link
      CSignal dom (BitVector 64)
    , -- \| Current state
      CSignal dom (SimplePeState bufferSize)
    )
switchDemoPeWb SNat = withMemoryMap mm $ Circuit go
 where
  go ((localCounter, wbM2S, dna, linkIn), _) = (((), wbS2M, (), ()), (linkOut, state))
   where
    readVec :: Vec (8 + bufferSize * 3 * 2 + 2) (Signal dom (BitVector 32))
    readVec =
      dflipflop
        <$> ( unbundle (bitCoerce . map swapWords <$> writableRegs)
                ++ unbundle (bitCoerce . map swapWords . bitCoerce <$> localCounter)
                ++ unbundle (bitCoerce . map swapWords <$> buffer)
            )

    (linkOut, buffer, state) =
      switchDemoPe
        (SNat @bufferSize)
        localCounter
        linkIn
        dna
        readStart
        readCycles
        writeStart
        writeCycles

    readStart = unpack <$> rs
    readCycles = checkedResize . bvToIndex <$> rc
    writeStart = unpack <$> ws
    writeCycles = checkedResize . bvToIndex <$> wc

    -- \| Unpack a BitVector to an Index of the same size
    bvToIndex :: (KnownNat n) => BitVector n -> Index (2 ^ n)
    bvToIndex = unpack

    -- \| Swap the two words of a 64-bit Bitvector to match the word order of
    -- the Vexriscv. This allows the CPU to read the two words as one 64-bit value.
    swapWords :: BitVector 64 -> BitVector 64
    swapWords = bitCoerce . (swap @(BitVector 32) @(BitVector 32)) . bitCoerce

    rs, rc, ws, wc :: Signal dom (BitVector 64)
    (rs, rc, ws, wc) = unbundle $ vecToTuple <$> writableRegs

    writableRegs :: Signal dom (Vec 4 (BitVector 64))
    writableRegs =
      (fmap (map swapWords . bitCoerce) . bundle . map (regMaybe maxBound) . unbundle)
        $ take d8
        <$> writeVec

    (writeVec, wbS2M) = unbundle $ wbToVec <$> bundle readVec <*> wbM2S

  mm =
    MemoryMap
      { tree = DeviceInstance locCaller "SwitchDemoPE"
      , deviceDefs = deviceSingleton deviceDef
      }
  deviceDef =
    DeviceDefinition
      { tags = []
      , registers =
          [ NamedLoc
              { name = Name "read_start" ""
              , loc = locHere
              , value =
                  Register
                    { fieldType = regType @(BitVector 64)
                    , address = 0x00
                    , access = WriteOnly
                    , tags = []
                    , reset = Nothing
                    }
              }
          , NamedLoc
              { name = Name "read_cycles" ""
              , loc = locHere
              , value =
                  Register
                    { fieldType = regType @(BitVector 64)
                    , address = 0x08
                    , access = WriteOnly
                    , tags = []
                    , reset = Nothing
                    }
              }
          , NamedLoc
              { name = Name "write_start" ""
              , loc = locHere
              , value =
                  Register
                    { fieldType = regType @(BitVector 64)
                    , address = 0x10
                    , access = WriteOnly
                    , tags = []
                    , reset = Nothing
                    }
              }
          , NamedLoc
              { name = Name "write_cycles" ""
              , loc = locHere
              , value =
                  Register
                    { fieldType = regType @(BitVector 64)
                    , address = 0x18
                    , access = WriteOnly
                    , tags = []
                    , reset = Nothing
                    }
              }
          , NamedLoc
              { name = Name "local_clock_cycle_counter" ""
              , loc = locHere
              , value =
                  Register
                    { fieldType = regType @(BitVector 64)
                    , address = 0x20
                    , access = ReadOnly
                    , tags = []
                    , reset = Nothing
                    }
              }
          , NamedLoc
              { name = Name "buffer" ""
              , loc = locHere
              , value =
                  Register
                    { fieldType = regType @(Vec (bufferSize * 3) (BitVector 64))
                    , address = 0x28
                    , access = ReadOnly
                    , tags = []
                    , reset = Nothing
                    }
              }
          ]
      , deviceName = Name "SwitchDemoPE" ""
      , definitionLoc = locHere
      }
