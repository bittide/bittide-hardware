-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Tests.AddressableBytesWb where

import Clash.Prelude

-- Local
import Bittide.Cpus.Riscv32imc (vexRiscv0)
import Bittide.DoubleBufferedRam (
  ContentType (Vec),
  blockRamByteAddressableU,
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
import Bittide.SharedTypes (withBittideByteOrder)
import Bittide.Wishbone (uartBytes, uartInterfaceWb)
import Project.FilePath (
  CargoBuildType (Release),
  findParentContaining,
  firmwareBinariesDir,
 )

import Clash.Class.BitPackC (ByteOrder (BigEndian))
import Data.Char (chr)
import Data.Constraint (Dict (..))
import Data.Constraint.Nat.Lemmas (cancelMulDiv)
import Data.Maybe (catMaybes)
import Protocols
import Protocols.Idle (idleSource)
import Protocols.MemoryMap (MemoryMap, Mm, getMMAny)
import Protocols.MemoryMap.Registers.WishboneStandard (
  deviceWb,
  registerConfig,
 )
import Protocols.MemoryMap.Registers.WishboneStandard.Internal (
  memoryWb,
 )
import Protocols.Wishbone (Wishbone, WishboneMode (Standard))
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty.HUnit (HasCallStack)
import VexRiscv (DumpVcd (DumpVcd, NoDumpVcd))

{- | Device that exposes an 8-word addressable buffer through Wishbone.
Demonstrates using memoryWb with blockRamByteAddressable to create memory-mapped RAM.
-}
addressableDeviceWb ::
  forall wordSize aw dom.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , KnownNat wordSize
  , KnownNat aw
  , 1 <= wordSize
  , 4 <= aw
  , ?busByteOrder :: ByteOrder
  , ?regByteOrder :: ByteOrder
  ) =>
  Circuit
    (ToConstBwd Mm, Wishbone dom 'Standard aw wordSize)
    ()
addressableDeviceWb = circuit $ \(mm, wb) -> do
  [buffer] <- deviceWb "AddressableBuffer" -< (mm, wb)

  -- Use memoryWb with blockRamByteAddressable primitive
  memoryWb hasClock hasReset (registerConfig "buffer") prim d8 -< buffer

  idC
 where
  -- BlockRam primitive with separated byte enables
  prim readAddr writeOp byteEnables =
    case cancelMulDiv @wordSize @8 of
      Dict -> blockRamByteAddressableU readAddr writeOp byteEnables

sim :: IO ()
sim = do
  -- Print the UART output to terminal for debugging
  putStrLn "=== UART Output ==="
  putStrLn simResult
  putStrLn "=================="

simResult :: String
simResult = chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream = sampleC def{timeoutAfter = 1000000} dut0

  dut0 :: Circuit () (Df Basic50 (BitVector 8))
  dut0 = Circuit $ ((),) . snd . toSignals dut . ((),) . snd

{- | An instance connecting a VexRiscv to a UART and an addressable buffer device.
Uses the Rust test binary.
-}
dut :: Circuit (ToConstBwd Mm) (Df Basic50 (BitVector 8))
dut = dutWithBinary "addressable_bytes_wb_test"

-- | Parameterized DUT that loads a specific firmware binary.
dutWithBinary ::
  (HasCallStack) =>
  String ->
  Circuit (ToConstBwd Mm) (Df Basic50 (BitVector 8))
dutWithBinary binaryName =
  withBittideByteOrder
    $ withClockResetEnable clockGen (resetGenN d2) enableGen
    $ circuit
    $ \mm -> do
      (uartRx, jtag) <- idleSource
      [uartBus, addressableBus] <- processingElement dumpVcd (peConfig binaryName) -< (mm, jtag)
      (uartTx, _uartStatus) <- uartInterfaceWb d2 d2 uartBytes -< (uartBus, uartRx)
      addressableDeviceWb -< addressableBus
      idC -< uartTx
 where
  dumpVcd =
    unsafePerformIO $ do
      mVal <- lookupEnv "ADDRESSABLE_BYTES_WB_DUMP_VCD"
      case mVal of
        Just s -> pure (DumpVcd s)
        _ -> pure NoDumpVcd

  peConfig binary = unsafePerformIO $ do
    root <- findParentContaining "cabal.project"
    let elfPath = root </> firmwareBinariesDir "riscv32imc" Release </> binary
    pure
      PeConfig
        { cpu = vexRiscv0
        , depthI = SNat @IMemWords
        , depthD = SNat @DMemWords
        , initI =
            Just
              $ Vec
              $ unsafePerformIO
              $ vecFromElfInstr BigEndian elfPath
        , initD =
            Just
              $ Vec
              $ unsafePerformIO
              $ vecFromElfData BigEndian elfPath
        , iBusTimeout = d0 -- No timeouts on the instruction bus
        , dBusTimeout = d0 -- No timeouts on the data bus
        , includeIlaWb = False
        }
{-# OPAQUE dutWithBinary #-}

memoryMap :: MemoryMap
memoryMap = getMMAny dut0
 where
  dut0 :: Circuit (ToConstBwd Mm, Df System ()) ()
  dut0 = circuit $ \(mm, _df) -> do
    _uart <- dut -< mm
    idC

type IMemWords = DivRU (256 * 1024) 4
type DMemWords = DivRU (256 * 1024) 4
