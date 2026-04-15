-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-orphans #-}

module Bittide.Instances.Tests.RingBuffer where

import Clash.Explicit.Prelude hiding (delayN)
import Clash.Prelude (
  HiddenClockResetEnable,
  delayN,
  hasClock,
  withClockResetEnable,
 )

import Clash.Cores.Xilinx.BlockRam (tdpbram)
import Data.Char (chr)
import Data.Maybe (catMaybes)
import GHC.Stack (HasCallStack)
import Project.FilePath
import Protocols
import Protocols.Df.Extra (tdpbramRamOp)
import Protocols.Idle
import Protocols.MemoryMap
import VexRiscv (DumpVcd (NoDumpVcd))

import Bittide.Cpus.Riscv32imc (vexRiscv0)
import Bittide.Instances.Common
import Bittide.ProcessingElement
import Bittide.RingBuffer
import Bittide.SharedTypes (withLittleEndian)
import Bittide.Wishbone

import qualified Data.List as L

createDomain vSystem{vName = "Slow", vPeriod = hzToPeriod 1_000_000}

-- | Memory depth for the ringbuffers (16 entries of 8 bytes each)
memDepth :: SNat 16
memDepth = SNat

dutMM :: (HasCallStack) => Protocols.MemoryMap.MemoryMap
dutMM =
  (\(SimOnly mm, _) -> mm)
    $ withClockResetEnable @Slow clockGen (resetGenN d2) enableGen
    $ toSignals
      (dutWithPeConfig d0 (emptyPeConfig (SNat @IMemWords) (SNat @DMemWords) d0 d0 False vexRiscv0))
      ((), pure $ deepErrorX "memoryMap")

-- | Parameterized DUT that loads a specific firmware binary with configurable latency.
dutWithPeConfig ::
  (HasCallStack, HiddenClockResetEnable dom, 1 <= DomainPeriod dom, KnownNat latency) =>
  SNat latency ->
  PeConfig 6 ->
  Circuit (ToConstBwd Mm) (Df dom (BitVector 8))
dutWithPeConfig latency peConfig = withLittleEndian $ circuit $ \mm -> do
  (uartRx, jtagIdle) <- idleSource
  [uartBus, wbTx, wbRx, timeBus] <-
    processingElement NoDumpVcd peConfig -< (mm, jtagIdle)
  (uartTx, _uartStatus) <- uartInterfaceWb d16 d2 uartBytes -< (uartBus, uartRx)
  txOut <- transmitRingBuffer (tdpbramRamOp tdpbram hasClock hasClock) memDepth -< wbTx
  txOutDelayed <- applyC (toSignal . delayN latency 0 . fromSignal) id -< txOut
  receiveRingBuffer (\ena -> blockRam hasClock ena (replicate memDepth 0)) memDepth
    -< (wbRx, txOutDelayed)
  _cnt <- timeWb Nothing -< timeBus
  idC -< uartTx
{-# OPAQUE dutWithPeConfig #-}

type IMemWords = DivRU (64 * 1024) 4
type DMemWords = DivRU (64 * 1024) 4

peConfigFromBinaryName :: String -> IO (PeConfig 6)
peConfigFromBinaryName binaryName = do
  peConfigFromElf
    (SNat @IMemWords)
    (SNat @DMemWords)
    (NameOnly binaryName)
    Release
    d0
    d0
    False
    vexRiscv0

takeUntilList :: (Eq a) => [a] -> [a] -> [a]
takeUntilList _ [] = []
takeUntilList prefix xs@(y : ys)
  | prefix `L.isPrefixOf` xs = []
  | otherwise = y : takeUntilList prefix ys

-- RingBuffer test simulation
simRingBuffer :: IO ()
simRingBuffer = putStr =<< simResultRingBuffer d4

simResultRingBuffer :: forall latency. (HasCallStack, KnownNat latency) => SNat latency -> IO String
simResultRingBuffer latency = do
  peConfig <- peConfigFromBinaryName "ring_buffer_test"
  let
    dutNoMm = circuit $ do
      mm <- ignoreMM
      uartTx <-
        withClockResetEnable clockGen (resetGenN d2) enableGen
          $ (dutWithPeConfig @System latency peConfig)
          -< mm
      idC -< uartTx
    uartStream = sampleC def{timeoutAfter = 1_000_000} dutNoMm
    result = takeUntilList "=== Test Complete ===" $ chr . fromIntegral <$> catMaybes uartStream
  pure result
