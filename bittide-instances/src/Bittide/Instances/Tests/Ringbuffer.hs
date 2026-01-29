-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-orphans #-}
module Bittide.Instances.Tests.Ringbuffer where

import Clash.Explicit.Prelude
import Clash.Prelude (
  HiddenClockResetEnable,
  exposeEnable,
  hasClock,
  withClockResetEnable,
 )

import Clash.Class.BitPackC (ByteOrder (BigEndian))
import Data.Char (chr)
import Data.Maybe (catMaybes)
import GHC.Stack (HasCallStack)
import Project.FilePath
import Protocols
import Protocols.Idle
import Protocols.MemoryMap
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import VexRiscv (DumpVcd (NoDumpVcd))

import Bittide.Cpus.Riscv32imc (vexRiscv0)
import Bittide.DoubleBufferedRam
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.Ringbuffer
import Bittide.SharedTypes (withBittideByteOrder)
import Bittide.Wishbone

createDomain vSystem{vName = "Slow", vPeriod = hzToPeriod 1000000}

-- | Memory depth for the ringbuffers (16 entries of 8 bytes each)
memDepth :: SNat 16
memDepth = SNat

dutMM :: (HasCallStack) => Protocols.MemoryMap.MemoryMap
dutMM =
  (\(SimOnly mm, _) -> mm)
    $ withClockResetEnable @System clockGen (resetGenN d2) enableGen
    $ toSignals (dutWithBinary "") ((), pure $ deepErrorX "memoryMap")

-- | Parameterized DUT that loads a specific firmware binary.
dutWithBinary ::
  (HasCallStack, HiddenClockResetEnable dom, 1 <= DomainPeriod dom) =>
  String ->
  Circuit (ToConstBwd Mm) (Df dom (BitVector 8))
dutWithBinary binaryName = withBittideByteOrder $ circuit $ \mm -> do
  (uartRx, jtagIdle) <- idleSource
  [uartBus, wbTx, wbRx, timeBus] <-
    processingElement NoDumpVcd (peConfig binaryName) -< (mm, jtagIdle)
  (uartTx, _uartStatus) <- uartInterfaceWb d16 d2 uartBytes -< (uartBus, uartRx)
  txOut <-
    transmitRingbufferWb (exposeEnable $ blockRamByteAddressable (Vec (repeat 0))) memDepth
      -< wbTx
  receiveRingbufferWb (\ena -> blockRam hasClock ena (replicate memDepth 0)) memDepth
    -< (wbRx, txOut)
  _cnt <- timeWb Nothing -< timeBus
  idC -< uartTx
 where
  peConfig binary = unsafePerformIO $ do
    root <- findParentContaining "cabal.project"
    let
      elfDir = root </> firmwareBinariesDir "riscv32imc" Release
      elfPath = elfDir </> binary
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

type IMemWords = DivRU (300 * 1024) 4
type DMemWords = DivRU (256 * 1024) 4

-- Ringbuffer test simulation
simRingbuffer :: IO ()
simRingbuffer = putStr simResultRingbuffer

simResultRingbuffer :: (HasCallStack) => String
simResultRingbuffer = chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream = sampleC def{timeoutAfter = 200_000} dutNoMM

  dutNoMM :: (HasCallStack) => Circuit () (Df System (BitVector 8))
  dutNoMM = circuit $ do
    mm <- ignoreMM
    uartTx <-
      withClockResetEnable clockGen (resetGenN d2) enableGen
        $ (dutWithBinary "ringbuffer_test")
        -< mm
    idC -< uartTx
