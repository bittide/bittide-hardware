-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | A software test to verify we can program the external clock board from
software using a specialized memory-mapped SPI device. This device is tested
against a model, see `si5391Mock`.
-}
module Bittide.Instances.Tests.Si539xSpi where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClockResetEnable, withClockResetEnable)

import GHC.Stack (HasCallStack)
import Project.FilePath (
  CargoBuildType (Release),
  findParentContaining,
  firmwareBinariesDir,
 )
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

import Clash.Class.BitPackC (ByteOrder (BigEndian))
import Protocols
import Protocols.Idle (idleSource)
import Protocols.MemoryMap
import VexRiscv

import Bittide.ClockControl.Si539xSpi
import Bittide.DoubleBufferedRam
import Bittide.ProcessingElement (PeConfig (..), processingElement)
import Bittide.ProcessingElement.Util (vecFromElfData, vecFromElfInstr)
import Bittide.SharedTypes (withBittideByteOrder)
import Bittide.Wishbone (timeWb, uartBytes, uartInterfaceWb)

memoryMap :: (HasCallStack) => MemoryMap
memoryMap = getMMAny $ withClockResetEnable @System clockGen (resetGenN d2) enableGen circuitFn

circuitFn ::
  forall free.
  ( HasCallStack
  , HiddenClockResetEnable free
  , 1 <= DomainPeriod free
  ) =>
  Circuit
    ( "MM" ::: ConstBwd MM
    , "MISO" ::: CSignal free Bit
    )
    ( "UART_TX" ::: Df free (BitVector 8)
    , "SPI_DONE" ::: CSignal free Bool
    , ( "SCLK" ::: CSignal free Bool
      , "MOSI" ::: CSignal free Bit
      , "CSB" ::: CSignal free Bool
      )
    )
circuitFn = withBittideByteOrder $ circuit $ \(mm, miso) -> do
  (uartRx, jtag) <- idleSource
  [siBus, timeBus, uartBus] <- processingElement NoDumpVcd peConfig -< (mm, jtag)

  (Fwd spiDone, spiOut) <- si539xSpiDriverMM (SNat @(Microseconds 10)) -< (siBus, miso)
  Fwd _localCounter <- timeWb -< timeBus
  (uartTx, _uartStatus) <- uartInterfaceWb d16 d16 uartBytes -< (uartBus, uartRx)

  idC -< (uartTx, Fwd spiDone, spiOut)
 where
  peConfig = unsafePerformIO $ do
    root <- findParentContaining "cabal.project"
    let
      elfDir = root </> firmwareBinariesDir "riscv32imc" Release
      elfPath = elfDir </> "si5395_test"
    pure
      PeConfig
        { initI =
            Reloadable
              $ Vec
              $ unsafePerformIO
              $ vecFromElfInstr @IMemWords BigEndian elfPath
        , initD =
            Reloadable
              $ Vec
              $ unsafePerformIO
              $ vecFromElfData @DMemWords BigEndian elfPath
        , iBusTimeout = d0 -- No timeouts on the instruction bus
        , dBusTimeout = d0 -- No timeouts on the data bus
        , includeIlaWb = False
        }
{-# NOINLINE circuitFn #-}

type IMemWords = DivRU (64 * 1024) 4
type DMemWords = DivRU (64 * 1024) 4
