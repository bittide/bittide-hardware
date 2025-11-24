-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Tests.TimeWb where

import Clash.Prelude

import Bittide.Cpus.Riscv32imc (vexRiscv0)
import Bittide.DoubleBufferedRam hiding (registerWb)
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.SharedTypes (withBittideByteOrder)
import Bittide.Wishbone
import Project.FilePath

import Clash.Class.BitPackC (BitPackC, ByteOrder (BigEndian))
import Protocols
import Protocols.Idle
import Protocols.MemoryMap (ConstBwd, MM)
import Protocols.MemoryMap.TypeDescription
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import VexRiscv (DumpVcd (NoDumpVcd))

import qualified Protocols.MemoryMap as MM

data TestStatus = Running | Success | Fail
  deriving (Show, Eq, Generic, NFDataX, BitPack, BitPackC)
deriveTypeDescription ''TestStatus

whoAmID :: BitVector 32
whoAmID = $(makeWhoAmIdTh "time")

-- | Memory map for the C timer test
timeWbMm :: MM.MemoryMap
timeWbMm = mm
 where
  Circuit circFn =
    withClockResetEnable clockGen resetGen enableGen $ dutCpu @System
  (SimOnly mm, _) = circFn ((), pure (Ack False))

-- | DUT for C timer test - VexRiscv with UART and Timer peripherals
dutCpu ::
  (HiddenClockResetEnable dom, 1 <= DomainPeriod dom) =>
  Circuit (ConstBwd MM) (Df dom (BitVector 8))
dutCpu = withBittideByteOrder $ circuit $ \mm -> do
  (uartRx, jtag) <- idleSource
  [uartBus, (mmTime, timeBus)] <-
    processingElement NoDumpVcd peConfig -< (mm, jtag)
  (uartTx, _uartStatus) <- uartInterfaceWb d2 d2 uartBytes -< (uartBus, uartRx)
  _localCounter <- timeWb -< (mmTime, timeBus)
  idC -< uartTx
 where
  peConfig = unsafePerformIO $ do
    root <- findParentContaining "cabal.project"
    let elfPath = root </> firmwareBinariesDir "riscv32imc" Release </> "c_timer_wb"
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

type IMemWords = DivRU (64 * 1024) 4
type DMemWords = DivRU (32 * 1024) 4
