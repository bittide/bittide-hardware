-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS -fplugin=Protocols.Plugin #-}

module Bittide.ProcessingElement where

import Clash.Explicit.Prelude (unsafeOrReset)
import Clash.Prelude

import Protocols
import Protocols.Wishbone
import VexRiscv (CpuIn (..), CpuOut (..), DumpVcd, Jtag, JtagOut (debugReset), vexRiscv)

import Bittide.DoubleBufferedRam
import Bittide.SharedTypes
import Bittide.Wishbone

import Clash.Cores.Xilinx.Ila (Depth (D4096))

import qualified Data.ByteString as BS

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
    { memMapConfig :: MemoryMap nBusses
    -- ^ The 'MemoryMap' for the contained 'singleMasterInterconnect'.
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
    } ->
    PeConfig nBusses

{- | VexRiscV based RV32IMC core together with instruction memory, data memory and
'singleMasterInterconnect'.
-}
processingElement ::
  forall dom nBusses.
  (HiddenClockResetEnable dom) =>
  DumpVcd ->
  PeConfig nBusses ->
  Circuit
    (Jtag dom)
    (Vec (nBusses - 2) (Wishbone dom 'Standard (MappedBusAddrWidth 30 nBusses) (Bytes 4)))
processingElement dumpVcd PeConfig{memMapConfig, initI, initD, iBusTimeout, dBusTimeout} = circuit $ \jtagIn -> do
  (iBus0, dBus0) <- rvCircuit dumpVcd (pure low) (pure low) (pure low) -< jtagIn
  iBus1 <-
    ilaWb (SSymbol @"instructionBus") 2 D4096 onTransactionWb onTransactionWb -< iBus0
  dBus1 <-
    watchDogWb "dBus" iBusTimeout
      <| ilaWb (SSymbol @"dataBus") 2 D4096 onTransactionWb onTransactionWb
      -< dBus0
  ([iMemBus, dMemBus], extBusses) <-
    (splitAtC d2 <| singleMasterInterconnect memMapConfig) -< dBus1
  wbStorage initD -< dMemBus
  iBus2 <- removeMsb <| watchDogWb "iBus" dBusTimeout -< iBus1 -- XXX: <= This should be handled by an interconnect
  wbStorageDPC initI -< (iBus2, iMemBus)
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

-- | Conceptually the same as 'splitAt', but for 'Circuit's
splitAtC ::
  SNat left ->
  Circuit (Vec (left + right) a) (Vec left a, Vec right a)
splitAtC SNat = Circuit go
 where
  go (fwd, (bwdLeft, bwdRight)) = (bwd, (fwdLeft, fwdRight))
   where
    (fwdLeft, fwdRight) = splitAtI fwd
    bwd = bwdLeft ++ bwdRight

rvCircuit ::
  (HiddenClockResetEnable dom) =>
  DumpVcd ->
  Signal dom Bit ->
  Signal dom Bit ->
  Signal dom Bit ->
  Circuit
    (Jtag dom)
    ( Wishbone dom 'Standard 30 (Bytes 4)
    , Wishbone dom 'Standard 30 (Bytes 4)
    )
rvCircuit dumpVcd tInterrupt sInterrupt eInterrupt = Circuit go
 where
  go (jtagIn, (iBusIn, dBusIn)) = (jtagOut, (iBusWbM2S <$> cpuOut, dBusWbM2S <$> cpuOut))
   where
    tupToCoreIn (timerInterrupt, softwareInterrupt, externalInterrupt, iBusWbS2M, dBusWbS2M) =
      CpuIn{timerInterrupt, softwareInterrupt, externalInterrupt, iBusWbS2M, dBusWbS2M}
    rvIn = tupToCoreIn <$> bundle (tInterrupt, sInterrupt, eInterrupt, iBusIn, dBusIn)
    (cpuOut, jtagOut) = vexRiscv dumpVcd hasClock (hasReset `unsafeOrReset` jtagReset) rvIn jtagIn
    jtagReset = unsafeFromActiveHigh (delay False (bitToBool . debugReset <$> jtagOut))

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
