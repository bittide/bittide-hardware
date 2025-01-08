-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS -fplugin=Protocols.Plugin #-}

module Bittide.ProcessingElement where

import Clash.Explicit.Prelude hiding (delay)
import Clash.Prelude

import Protocols
import Protocols.Extra
import Protocols.Wishbone
import VexRiscv (CpuIn (..), CpuOut (..), DumpVcd, Jtag, vexRiscv)

import Bittide.DoubleBufferedRam
import Bittide.SharedTypes
import Bittide.Wishbone

import Clash.Class.BitPackC (ByteOrder (BigEndian, LittleEndian))
import Clash.Cores.Xilinx.Ila (Depth (D4096))

import qualified Data.ByteString as BS
import Data.String.Interpolate (i)
import qualified Protocols.MemoryMap as MM (
  ConstBwd,
  MM,
  constBwd,
  withDeviceTag,
  withTag,
 )

-- | Configuration for a Bittide Processing Element.
data PeConfig nBusses where
  PeConfig ::
    forall depthI depthD nBusses iBusTimeout dBusTimeout.
    ( KnownNat depthI
    , 1 <= depthI
    , KnownNat depthD
    , 1 <= depthD
    , KnownNat nBusses
    , 2 <= nBusses
    , CLog 2 nBusses <= 30
    ) =>
    { prefixI :: Unsigned (CLog 2 nBusses)
    -- ^ The prefix of the address of the instruction bus
    , prefixD :: Unsigned (CLog 2 nBusses)
    -- ^ The prefix of the address of the data bus
    , initI :: InitialContent depthI (Bytes 4)
    -- ^ Initial content of the instruction memory, can be smaller than its total depth.
    , initD :: InitialContent depthD (Bytes 4)
    -- ^ Initial content of the data memory, can be smaller than its total depth.
    , iBusTimeout :: SNat iBusTimeout
    -- ^ Number of clock cycles after which the a transaction on the instruction bus times out.
    -- Set to 0 to disable timeouts on the instruction bus.
    , dBusTimeout :: SNat dBusTimeout
    -- ^ Number of clock cycles after which the a transaction on the data bus times out.
    -- Set to 0 to disable timeouts on the data bus.
    , includeIlaWb :: Bool
    -- ^ Indicates whether or not to include the Wishbone ILA component probes. Should be set
    -- to 'False' if this CPU is not in an always-on domain. Additionally, only one CPU in any
    -- given system should have this set to 'True' in order to avoid probe name conflicts.
    } ->
    PeConfig nBusses

{- | VexRiscV based RV32IMC core together with instruction memory, data memory and
'singleMasterInterconnect'.
-}
processingElement ::
  forall dom nBusses.
  ( HiddenClockResetEnable dom
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  , KnownNat nBusses
  , 2 <= nBusses
  ) =>
  DumpVcd ->
  PeConfig nBusses ->
  Circuit
    (MM.ConstBwd MM.MM, Jtag dom)
    ( Vec
        (nBusses - 2)
        ( MM.ConstBwd (Unsigned (CLog 2 nBusses))
        , (MM.ConstBwd MM.MM, Wishbone dom 'Standard (MappedBusAddrWidth 30 nBusses) (Bytes 4))
        )
    )
processingElement dumpVcd PeConfig{prefixI, prefixD, initI, initD, iBusTimeout, dBusTimeout, includeIlaWb} = circuit $ \(mm, jtagIn) -> do
  (iBus0, (mmDbus, dBus0)) <-
    rvCircuit dumpVcd (pure low) (pure low) (pure low) -< (mm, jtagIn)
  iBus1 <-
    maybeIlaWb
      includeIlaWb
      (SSymbol @"instructionBus")
      2
      D4096
      onTransactionWb
      onTransactionWb
      -< iBus0
  dBus1 <-
    watchDogWb "dBus" iBusTimeout
      <| maybeIlaWb
        includeIlaWb
        (SSymbol @"dataBus")
        2
        D4096
        onTransactionWb
        onTransactionWb
      -< dBus0
  ([(iPre, (mmI, iMemBus)), (dPre, (mmD, dMemBus))], extBusses) <-
    (splitAtCI <| singleMasterInterconnectC) -< (mmDbus, dBus1)
  MM.constBwd prefixD -< dPre
  MM.constBwd prefixI -< iPre

  -- Instruction and data memory are never accessed explicitly by developers,
  -- only implicitly by the CPU itself. We therefore don't need to generate HAL
  -- code. We instruct the generator to skip them by adding a "no-generate" tag.
  MM.withTag "no-generate"
    $ MM.withDeviceTag "no-generate"
    $ wbStorage "DataMemory" initD
    -< (mmD, dMemBus)

  iBus2 <- removeMsb <| watchDogWb "iBus" dBusTimeout -< iBus1 -- XXX: <= This should be handled by an interconnect
  MM.withTag "no-generate"
    $ MM.withDeviceTag "no-generate"
    $ wbStorageDPC "InstructionMemory" initI
    -< (mmI, (iBus2, iMemBus))

  idC -< extBusses
 where
  removeMsb ::
    forall aw a.
    (KnownNat aw) =>
    Circuit
      (Wishbone dom 'Standard (aw + 4) a)
      (Wishbone dom 'Standard aw a)
  removeMsb = wbMap (mapAddr (truncateB :: BitVector (aw + 4) -> BitVector aw)) id

  wbMap fwd bwd = Circuit $ \(m2s, s2m) -> (fmap bwd s2m, fmap fwd m2s)

rvCircuit ::
  ( HiddenClockResetEnable dom
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  DumpVcd ->
  Signal dom Bit ->
  Signal dom Bit ->
  Signal dom Bit ->
  Circuit
    (MM.ConstBwd MM.MM, Jtag dom)
    ( Wishbone dom 'Standard 30 (Bytes 4)
    , (MM.ConstBwd MM.MM, Wishbone dom 'Standard 30 (Bytes 4))
    )
rvCircuit dumpVcd tInterrupt sInterrupt eInterrupt =
  case (?busByteOrder, ?regByteOrder) of
    (BigEndian, LittleEndian) -> Circuit go
    (busByteOrder, regByteOrder) ->
      clashCompileError
        [i|
          Unsupported bus and register byte order combination:

            busByteOrder = #{show busByteOrder}
            regByteOrder = #{show regByteOrder}

          The only supported combination is:

            busByteOrder = BigEndian
            regByteOrder = LittleEndian
        |]
 where
  go (((), jtagIn), (iBusIn, (mm, dBusIn))) = ((mm, jtagOut), (iBusWbM2S <$> cpuOut, ((), dBusWbM2S <$> cpuOut)))
   where
    tupToCoreIn (timerInterrupt, softwareInterrupt, externalInterrupt, iBusWbS2M, dBusWbS2M) =
      CpuIn{timerInterrupt, softwareInterrupt, externalInterrupt, iBusWbS2M, dBusWbS2M}
    rvIn = tupToCoreIn <$> bundle (tInterrupt, sInterrupt, eInterrupt, iBusIn, dBusIn)
    (cpuOut, jtagOut) = vexRiscv dumpVcd hasClock (hasReset `unsafeOrReset` jtagReset) rvIn jtagIn
    jtagReset = unsafeFromActiveHigh (delay False (bitToBool . ndmreset <$> cpuOut))

-- | Map a function over the address field of 'WishboneM2S'
mapAddr ::
  (BitVector aw1 -> BitVector aw2) ->
  WishboneM2S aw1 selWidth a ->
  WishboneM2S aw2 selWidth a
mapAddr f wb = wb{addr = f (addr wb)}

{- | Provide a vector of filepaths, and a write operations containing a byteSelect and
a vector of characters and, for each filepath write the corresponding byte to that file
if the corresponding byteSelect is @1@.
-}
printCharacters ::
  (KnownNat paths, KnownNat chars, (paths + n) ~ chars) =>
  -- | Destination files for received bytes.
  Vec paths FilePath ->
  -- | Write attempt, bytes will only be written if the corresponding byteSelect is @1@.
  Maybe (BitVector chars, Vec chars Byte) ->
  IO ()
printCharacters Nil _ = pure ()
printCharacters paths@(Cons _ _) inps = case inps of
  Just (byteSelect, chars) ->
    sequence_ $ printToFiles <*> take SNat (unpack byteSelect) <*> take SNat chars
  Nothing -> pure ()
 where
  printToFiles = printToFile <$> paths
  printToFile path byteSelect char
    | byteSelect = BS.appendFile path $ BS.singleton $ bitCoerce char
    | otherwise = pure ()
