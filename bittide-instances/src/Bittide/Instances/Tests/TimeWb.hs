-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Tests.TimeWb where

import Clash.Prelude

import Bittide.Cpus.Riscv32imc (vexRiscv0)
import Bittide.DoubleBufferedRam hiding (registerWb)
import Bittide.Instances.Hitl.Utils.Driver (buildRustTarget)
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.SharedTypes (withLittleEndian)
import Bittide.Wishbone
import Project.FilePath

import Clash.Class.BitPackC (BitPackC, ByteOrder (BigEndian))
import Protocols
import Protocols.Idle
import Protocols.MemoryMap (Mm)
import Protocols.MemoryMap.TypeDescription
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import VexRiscv (DumpVcd (NoDumpVcd))

import qualified Protocols.MemoryMap as Mm

data TestStatus = Running | Success | Fail
  deriving (Show, Eq, Generic, NFDataX, BitPack, BitPackC)
deriveTypeDescription ''TestStatus

-- | Memory map for the C timer test
timeWbMm :: Mm.MemoryMap
timeWbMm = mm
 where
  Circuit circFn =
    withClockResetEnable clockGen resetGen enableGen $ dutCpu @System
  (SimOnly mm, _) = circFn ((), pure (Ack False))

-- | DUT for C timer test - VexRiscv with UART and Timer peripherals
dutCpu ::
  (HiddenClockResetEnable dom, 1 <= DomainPeriod dom) =>
  Circuit (ToConstBwd Mm) (Df dom (BitVector 8))
dutCpu = withLittleEndian $ circuit $ \mm -> do
  (uartRx, jtag) <- idleSource
  [uartBus, (mmTime, timeBus)] <-
    processingElement NoDumpVcd peConfig -< (mm, jtag)
  (uartTx, _uartStatus) <- uartInterfaceWb d2 d2 uartBytes -< (uartBus, uartRx)
  _localCounter <- timeWb Nothing -< (mmTime, timeBus)
  idC -< uartTx
 where
  peConfig = unsafePerformIO $ do
    root <- findParentContaining "cabal.project"
    let
      binName = "c_timer_wb"
      buildType = Release
      runBuild = buildRustTarget root binName buildType
      elfPath = root </> firmwareBinariesDir "riscv32imc" buildType </> "c_timer_wb"
    pure
      PeConfig
        { cpu = vexRiscv0
        , depthI = SNat @IMemWords
        , depthD = SNat @DMemWords
        , initI =
            Just
              $ Vec
              $ unsafePerformIO
              $ runBuild
              >> vecFromElfInstr BigEndian elfPath
        , initD =
            Just
              $ Vec
              $ unsafePerformIO
              $ runBuild
              >> vecFromElfData BigEndian elfPath
        , iBusTimeout = d0
        , dBusTimeout = d0
        , includeIlaWb = False
        }

type IMemWords = DivRU (64 * 1024) 4
type DMemWords = DivRU (32 * 1024) 4
