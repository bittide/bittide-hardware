-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.Tests.ClockControlWb where

import Clash.Prelude

import Bittide.ClockControl.Registers (ClockControlData, clockControlWb)
import Bittide.Cpus.Riscv32imc (vexRiscv0)
import Bittide.DoubleBufferedRam
import Bittide.Instances.Hitl.Setup hiding (linkMask)
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.SharedTypes (withBittideByteOrder)
import Bittide.Wishbone hiding (MemoryMap)
import Clash.Class.BitPackC (ByteOrder (BigEndian))
import GHC.Stack (HasCallStack)
import Project.FilePath
import Protocols
import Protocols.Idle
import Protocols.MemoryMap
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import VexRiscv (DumpVcd (NoDumpVcd))

linkMask :: BitVector LinkCount
linkMask = 0b1011011

linksOk :: BitVector LinkCount
linksOk = 0b1111000

dataCounts :: Vec LinkCount (Signed 32)
dataCounts = iterateI (satSucc SatWrap) 0

type IMemWords = DivRU (64 * 1024) 4
type DMemWords = DivRU (32 * 1024) 4

dut ::
  (HasCallStack) =>
  Circuit (ToConstBwd Mm) (Df System (BitVector 8), CSignal System (ClockControlData LinkCount))
dut =
  withBittideByteOrder
    $ withClockResetEnable clockGen (resetGenN d2) enableGen
    $ circuit
    $ \mm -> do
      (uartRx, jtag) <- idleSource
      [uartBus, (mmCC, ccWb)] <- processingElement NoDumpVcd peConfig -< (mm, jtag)
      (uartTx, _uartStatus) <- uartInterfaceWb d2 d2 uartBytes -< (uartBus, uartRx)

      ccd <-
        clockControlWb
          (pure linkMask)
          (pure linksOk)
          (pure <$> dataCounts)
          -< (mmCC, ccWb)

      idC -< (uartTx, ccd)
 where
  peConfig = unsafePerformIO $ do
    root <- findParentContaining "cabal.project"
    let
      elfDir = root </> firmwareBinariesDir "riscv32imc" Release
      elfPath = elfDir </> "clock-control-wb"
    pure
      PeConfig
        { cpu = vexRiscv0
        , initI =
            Reloadable @IMemWords
              $ Vec
              $ unsafePerformIO
              $ vecFromElfInstr BigEndian elfPath
        , initD =
            Reloadable @DMemWords
              $ Vec
              $ unsafePerformIO
              $ vecFromElfData BigEndian elfPath
        , iBusTimeout = d0
        , dBusTimeout = d0
        , includeIlaWb = False
        }
{-# OPAQUE dut #-}

dutMm :: (HasCallStack) => MemoryMap
dutMm = mm
 where
  Circuit circuitFn = dut
  (SimOnly mm, _) = circuitFn ((), (pure $ deepErrorX "uart_bwd", ()))

dutNoMm ::
  (HasCallStack) => Circuit () (Df System (BitVector 8), CSignal System (ClockControlData LinkCount))
dutNoMm = Circuit circuitFnNoMm
 where
  Circuit circuitFn = dut
  circuitFnNoMm (fwdL, bwdR) = let (_, fwdR) = circuitFn (fwdL, bwdR) in ((), fwdR)
