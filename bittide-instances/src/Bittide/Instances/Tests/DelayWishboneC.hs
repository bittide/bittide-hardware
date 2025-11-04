-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Tests.DelayWishboneC where

import Clash.Prelude hiding (unzip)

import Bittide.DoubleBufferedRam
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.SharedTypes (withBittideByteOrder)
import Bittide.Wishbone hiding (MemoryMap)
import Project.FilePath

import Clash.Class.BitPackC (ByteOrder (BigEndian))
import Protocols
import Protocols.Idle
import Protocols.MemoryMap (ConstBwd, MM, MemoryMap)
import Protocols.Vec (unzip)
import Protocols.Wishbone.Extra
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import VexRiscv (DumpVcd (NoDumpVcd))

whoAmID :: BitVector 32
whoAmID = $(makeWhoAmIdTh "helo")

delayMm :: MemoryMap
delayMm = mm
 where
  Circuit circFn =
    withClockResetEnable clockGen resetGen enableGen $ dutCpu @System
  (SimOnly mm, _) = circFn ((), pure (Ack False))

dutCpu ::
  (HiddenClockResetEnable dom, 1 <= DomainPeriod dom) =>
  Circuit (ConstBwd MM) (Df dom (BitVector 8))
dutCpu = withBittideByteOrder $ circuit $ \mm -> do
  (uartRx, jtag) <- idleSource
  mmWbs <- processingElement NoDumpVcd peConfig -< (mm, jtag)
  ([whoAmIMm, uartMm], wbs) <- unzip -< mmWbs
  [whoAmIBus, uartBus] <- repeatC delayWishboneC -< wbs
  whoAmIC whoAmID -< (whoAmIMm, whoAmIBus)
  (uartTx, _uartStatus) <- uartInterfaceWb d2 d2 uartBytes -< ((uartMm, uartBus), uartRx)
  idC -< uartTx
 where
  peConfig = unsafePerformIO $ do
    root <- findParentContaining "cabal.project"
    let elfPath = root </> firmwareBinariesDir "riscv32imc" Release </> "delay_wishbone_c"
    pure
      PeConfig
        { initI =
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
