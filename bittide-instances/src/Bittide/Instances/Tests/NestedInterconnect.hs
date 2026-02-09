-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | Regression test to make sure that nested interconnects work properly
with memory map generation.
-}
module Bittide.Instances.Tests.NestedInterconnect where

import Clash.Prelude

import Bittide.Cpus.Riscv32imc (vexRiscv0)
import Bittide.DoubleBufferedRam (
  ContentType (Vec),
 )
import Bittide.Instances.Domains (Basic50)
import Bittide.ProcessingElement (
  PeConfig (..),
  processingElement,
 )
import Bittide.ProcessingElement.Util (
  vecFromElfData,
  vecFromElfInstr,
 )
import Bittide.SharedTypes (Bytes, withBittideByteOrder)
import Bittide.Wishbone (singleMasterInterconnectC, uartBytes, uartInterfaceWb)
import Project.FilePath (
  CargoBuildType (Release),
  findParentContaining,
  firmwareBinariesDir,
 )

import Clash.Class.BitPackC (ByteOrder (BigEndian))
import Clash.Sized.Vector.Extra
import Data.Char (chr)
import Data.Maybe (catMaybes)
import Protocols
import Protocols.Idle (idleSink, idleSource)
import Protocols.MemoryMap (MemoryMap, Mm, ignoreMM, withName)
import Protocols.MemoryMap.Registers.WishboneStandard (
  deviceWb,
  registerConfig,
  registerWb_,
 )
import Protocols.Wishbone (Wishbone, WishboneMode (Standard))
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty.HUnit (HasCallStack)
import VexRiscv (DumpVcd (NoDumpVcd))

import qualified Protocols.ToConst as ToConst
import qualified Protocols.Vec as Vec

-- | Simple peripheral device with two registers (status and control)
simplePeripheral ::
  forall dom aw.
  ( HasCallStack
  , HiddenClock dom
  , HiddenReset dom
  , KnownNat aw
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  String ->
  Circuit
    (ToConstBwd Mm, Wishbone dom 'Standard aw (Bytes 4))
    ()
simplePeripheral name = withName name $ circuit $ \(mm, wb) -> do
  [(offset0, meta0, wb0), (offset1, meta1, wb1)] <- deviceWb "somePeripheral" -< (mm, wb)
  registerWb_ hasClock hasReset (registerConfig "status") (0 :: Unsigned 32)
    -< ((offset0, meta0, wb0), Fwd noWrite)
  registerWb_ hasClock hasReset (registerConfig "control") (0 :: Unsigned 32)
    -< ((offset1, meta1, wb1), Fwd noWrite)
 where
  noWrite = pure Nothing

-- | Processing element configuration
peConfig :: PeConfig 7
peConfig = unsafePerformIO $ do
  root <- findParentContaining "cabal.project"
  let elfPath = root </> firmwareBinariesDir "riscv32imc" Release </> "nested_interconnect_test"
  pure
    PeConfig
      { cpu = vexRiscv0
      , depthI = SNat @IMemWords
      , depthD = SNat @DMemWords
      , initI =
          Just
            $ Vec @IMemWords
            $ unsafePerformIO
            $ vecFromElfInstr BigEndian elfPath
      , initD =
          Just
            $ Vec @DMemWords
            $ unsafePerformIO
            $ vecFromElfData BigEndian elfPath
      , iBusTimeout = d0
      , dBusTimeout = d0
      , includeIlaWb = False
      }

-- | DUT with nested singleMasterInterconnect (for memory map generation)
dut ::
  (HiddenClockResetEnable dom, 1 <= DomainPeriod dom) =>
  Circuit (ToConstBwd Mm) (Df dom (BitVector 8))
dut = withBittideByteOrder $ circuit $ \mm -> do
  (uartRx, jtag) <- idleSource
  ([uartBus, preInterconnectBus0], unNestedBusses) <-
    Vec.split
      <| processingElement NoDumpVcd peConfig
      -< (mm, jtag)
  (uartTx, _uartStatus) <- uartInterfaceWb d2 d2 uartBytes -< (uartBus, uartRx)

  -- Nested device with multiple peripherals
  idleSink
    <| (Vec.vecCircuits (repeat (simplePeripheral "unnestedPeripherals")))
    -< unNestedBusses

  (pfxs0, nestedBusses0) <- Vec.unzip <| singleMasterInterconnectC -< preInterconnectBus0
  ([preInterconnectBus1], nestedBusses1) <- Vec.split -< nestedBusses0
  idleSink <| (Vec.vecCircuits $ fmap ToConst.toBwd prefixes0) -< pfxs0
  idleSink
    <| (Vec.vecCircuits (replicate d3 (simplePeripheral "L1NestedPeripherals")))
    -< nestedBusses1

  (pfxs1, nestedBusses2) <- Vec.unzip <| singleMasterInterconnectC -< preInterconnectBus1
  idleSink <| (Vec.vecCircuits $ fmap ToConst.toBwd prefixes1) -< pfxs1
  idleSink
    <| (Vec.vecCircuits (replicate d3 (simplePeripheral "L2NestedPeripherals")))
    -< nestedBusses2
  idC -< uartTx
 where
  prefixes0 = incrementWithBlacklist @_ @_ @4 Nil
  prefixes1 = incrementWithBlacklist @_ @_ @4 Nil

-- | DUT for simulation (uses ignoreMM)
top :: Circuit () (Df Basic50 (BitVector 8))
top = withBittideByteOrder
  $ withClockResetEnable clockGen (resetGenN d2) enableGen
  $ circuit
  $ \_unit -> do
    mm <- ignoreMM
    dut -< mm

type IMemWords = DivRU (64 * 1024) 4
type DMemWords = DivRU (64 * 1024) 4

-- | Memory map for the nested interconnect test
nestedInterconnectMm :: MemoryMap
nestedInterconnectMm = mm
 where
  Circuit circFn =
    withClockResetEnable clockGen resetGen enableGen $ dut @System
  (SimOnly mm, _) = circFn ((), pure (Ack False))

-- | Simulation result
simResult :: String
simResult = chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream = sampleC def{timeoutAfter = 500_000} top
