-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Tests.AddressableBytesWb where

import Clash.Prelude

-- Local
import Bittide.Cpus.Riscv32imc (vexRiscv0)
import Bittide.DoubleBufferedRam (
  ContentType (Vec),
  wbStorage,
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
import Bittide.SharedTypes (withByteOrder)
import Bittide.Wishbone (uartBytes, uartInterfaceWb)
import Project.FilePath (
  CargoBuildType (Release),
  findParentContaining,
  firmwareBinariesDir,
 )

import Clash.Class.BitPackC (ByteOrder (BigEndian, LittleEndian))
import Data.Char (chr)
import Data.Maybe (catMaybes)
import Protocols
import Protocols.Idle (idleSource)
import Protocols.MemoryMap (MemoryMap, Mm, getMMAny)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty.HUnit (HasCallStack)
import VexRiscv (DumpVcd (DumpVcd, NoDumpVcd))

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
  withByteOrder LittleEndian
    $ withClockResetEnable clockGen (resetGenN d2) enableGen
    $ circuit
    $ \mm -> do
      (uartRx, jtag) <- idleSource
      [uartBus, addressableBus] <- processingElement dumpVcd (peConfig binaryName) -< (mm, jtag)
      (uartTx, _uartStatus) <- uartInterfaceWb d2 d2 uartBytes -< (uartBus, uartRx)
      wbStorage "AddressableBuffer" d8 Nothing -< addressableBus
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
