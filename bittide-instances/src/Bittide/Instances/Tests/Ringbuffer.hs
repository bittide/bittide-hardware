-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-orphans #-}

module Bittide.Instances.Tests.Ringbuffer where

import Clash.Explicit.Prelude hiding (delayN)
import Clash.Prelude (
  HiddenClockResetEnable,
  delayN,
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
import Protocols.Extra (fmapC)
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

import qualified Data.List as L
import qualified Protocols.Vec as Vec

createDomain vSystem{vName = "Slow", vPeriod = hzToPeriod 1000000}

-- | Memory depth for the ringbuffers (16 entries of 8 bytes each)
memDepth :: SNat 16
memDepth = SNat

dutMM :: (HasCallStack) => Protocols.MemoryMap.MemoryMap
dutMM =
  (\(SimOnly mm, _) -> mm)
    $ withClockResetEnable @Slow clockGen (resetGenN d2) enableGen
    $ toSignals (dutWithBinary d0 "") ((), pure $ deepErrorX "memoryMap")

replicateC :: forall n a b. SNat n -> Circuit a b -> Circuit (Vec n a) (Vec n b)
replicateC SNat c = repeatC c

-- | Parameterized DUT that loads a specific firmware binary with configurable latency.
dutWithBinary ::
  (HasCallStack, HiddenClockResetEnable dom, 1 <= DomainPeriod dom, KnownNat latency) =>
  SNat latency ->
  String ->
  Circuit (ToConstBwd Mm) (Df dom (BitVector 8))
dutWithBinary latency binaryName = withBittideByteOrder $ circuit $ \mm -> do
  (uartRx, jtagIdle) <- idleSource
  ([uartBus, timeBus], wbTxs, wbRxs) <-
    Vec.split3
      <| processingElement NoDumpVcd (peConfig binaryName)
      -< (mm, jtagIdle)
  (uartTx, _uartStatus) <- uartInterfaceWb d16 d2 uartBytes -< (uartBus, uartRx)
  txOuts <-
    replicateC
      d2
      ( transmitRingbufferWb (exposeEnable $ blockRamByteAddressable (Vec (repeat 0))) memDepth
      )
      -< wbTxs
  -- Add configurable latency between TX and RX ringbuffers
  txOutDelayeds <- fmapC (applyC (toSignal . delayN latency 0 . fromSignal) id) -< txOuts
  idleSink
    <| fmapC (receiveRingbufferWb (\ena -> blockRam hasClock ena (replicate memDepth 0)) memDepth)
    <| Vec.zip
    -< (wbRxs, txOutDelayeds)
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

takeUntilList :: (Eq a) => [a] -> [a] -> [a]
takeUntilList _ [] = []
takeUntilList prefix xs@(y : ys)
  | prefix `L.isPrefixOf` xs = []
  | otherwise = y : takeUntilList prefix ys

-- Ringbuffer test simulation
simRingbuffer :: IO ()
simRingbuffer = putStr $ simResultRingbuffer d0

simResultRingbuffer :: forall latency. (HasCallStack, KnownNat latency) => SNat latency -> String
simResultRingbuffer lat = takeUntilList "=== Test Complete ===" $ chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream = sampleC def{timeoutAfter = 1_000_000} (dutNoMM lat)

  dutNoMM :: (HasCallStack, KnownNat n) => SNat n -> Circuit () (Df Slow (BitVector 8))
  dutNoMM latency = circuit $ do
    mm <- ignoreMM
    uartTx <-
      withClockResetEnable clockGen (resetGenN d2) enableGen
        $ (dutWithBinary latency "ringbuffer_test")
        -< mm
    idC -< uartTx

simSmolTcp :: IO ()
simSmolTcp = putStr $ simResultSmolTcp d0

simResultSmolTcp :: forall latency. (HasCallStack, KnownNat latency) => SNat latency -> String
simResultSmolTcp lat = takeUntilList "=== Test Complete ===" $ chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream = sampleC def{timeoutAfter = 10_000_000} (dutNoMM lat)

  dutNoMM :: (HasCallStack, KnownNat n) => SNat n -> Circuit () (Df Slow (BitVector 8))
  dutNoMM latency = circuit $ do
    mm <- ignoreMM
    uartTx <-
      withClockResetEnable clockGen (resetGenN d2) enableGen
        $ (dutWithBinary latency "ringbuffer_smoltcp_test")
        -< mm
    idC -< uartTx

simAlignedRingbuffer :: IO ()
simAlignedRingbuffer = putStr $ simResultAlignedRingbuffer d0

simResultAlignedRingbuffer ::
  forall latency. (HasCallStack, KnownNat latency) => SNat latency -> String
simResultAlignedRingbuffer lat = takeUntilList "=== Test Complete ===" $ chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream = sampleC def{timeoutAfter = 250_000} (dutNoMM lat)

  dutNoMM :: (HasCallStack, KnownNat n) => SNat n -> Circuit () (Df Slow (BitVector 8))
  dutNoMM latency = circuit $ do
    mm <- ignoreMM
    uartTx <-
      withClockResetEnable clockGen (resetGenN d2) enableGen
        $ (dutWithBinary latency "aligned_ringbuffer_test")
        -< mm
    idC -< uartTx
