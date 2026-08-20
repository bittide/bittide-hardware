-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | Simulation DUT for the decode demo's software engines (variants C and
A′ in @firmware-support/bittide-sys/src/decode_demo.rs@, exercised by
@firmware-binaries/sim-tests/decode_loop_test@).

One CPU with two transmit/receive ring-buffer pairs, each pair looped back
through a configurable-latency delay line — a one-node ring: the forward
pair carries frames (the node's own contribution returns as the
"accumulated" vector), the second pair carries the acks. Depth 512, the
decode demo's ring-buffer depth, so the firmware measures its per-frame
service time against the real wrap window.
-}
module Bittide.Instances.Tests.DecodeLoop where

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
import Protocols.Experimental.Simulate (SimulationConfig (..), sampleC)
import Protocols.Idle
import Protocols.MemoryMap
import VexRiscv (DumpVcd (NoDumpVcd))

import Bittide.Cpus.Riscv32imc (vexRiscv0)
import Bittide.Instances.Common
import Bittide.Instances.Tests.RingBuffer (takeUntilList)
import Bittide.ProcessingElement
import Bittide.RingBuffer
import Bittide.SharedTypes (withLittleEndian)
import Bittide.Wishbone

createDomain vSystem{vName = "SlowDecodeLoop", vPeriod = hzToPeriod 1_000_000}

-- | The decode demo's ring-buffer depth.
memDepth :: SNat 512
memDepth = SNat

dutMM :: (HasCallStack) => Protocols.MemoryMap.MemoryMap
dutMM =
  (\(SimOnly mm, _) -> mm)
    $ withClockResetEnable @SlowDecodeLoop clockGen (resetGenN d2) enableGen
    $ toSignals
      (dutWithPeConfig d0 (emptyPeConfig (SNat @IMemWords) (SNat @DMemWords) d0 d0 False vexRiscv0))
      ((), pure $ deepErrorX "memoryMap")

{- | Two ring-buffer pairs looped back through delay lines: pair 0 (the
forward path) and pair 1 (the ack path).
-}
dutWithPeConfig ::
  (HasCallStack, HiddenClockResetEnable dom, 1 <= DomainPeriod dom, KnownNat latency) =>
  SNat latency ->
  PeConfig 8 ->
  Circuit (ToConstBwd Mm) (Df dom (BitVector 8))
dutWithPeConfig latency peConfig = withLittleEndian $ circuit $ \mm -> do
  (uartRx, jtagIdle) <- idleSource
  [uartBus, wbTx0, wbTx1, wbRx0, wbRx1, timeBus] <-
    processingElement NoDumpVcd peConfig -< (mm, jtagIdle)
  (uartTx, _uartStatus) <- uartInterfaceWb d16 d2 uartBytes -< (uartBus, uartRx)
  tx0Out <- transmitRingBuffer (tdpbramRamOp tdpbram hasClock hasClock) memDepth -< wbTx0
  tx1Out <- transmitRingBuffer (tdpbramRamOp tdpbram hasClock hasClock) memDepth -< wbTx1
  tx0Delayed <- applyC (toSignal . delayN latency 0 . fromSignal) id -< tx0Out
  tx1Delayed <- applyC (toSignal . delayN latency 0 . fromSignal) id -< tx1Out
  receiveRingBuffer (\ena -> blockRam hasClock ena (replicate memDepth 0)) memDepth
    -< (wbRx0, tx0Delayed)
  receiveRingBuffer (\ena -> blockRam hasClock ena (replicate memDepth 0)) memDepth
    -< (wbRx1, tx1Delayed)
  _cnt <- timeWb Nothing -< timeBus
  idC -< uartTx
{-# OPAQUE dutWithPeConfig #-}

type IMemWords = DivRU (64 * 1024) 4
type DMemWords = DivRU (64 * 1024) 4

peConfigFromBinaryName :: String -> IO (PeConfig 8)
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

simDecodeLoop :: IO ()
simDecodeLoop = putStr =<< simResultDecodeLoop d34

simResultDecodeLoop ::
  forall latency. (HasCallStack, KnownNat latency) => SNat latency -> IO String
simResultDecodeLoop latency = do
  peConfig <- peConfigFromBinaryName "decode_loop_test"
  let
    dutNoMm = circuit $ do
      mm <- ignoreMM
      uartTx <-
        withClockResetEnable clockGen (resetGenN d2) enableGen
          $ (dutWithPeConfig @System latency peConfig)
          -< mm
      idC -< uartTx
    uartStream = sampleC def{timeoutAfter = 5_000_000} dutNoMm
    result = takeUntilList "=== Test Complete ===" $ chr . fromIntegral <$> catMaybes uartStream
  pure result
