-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.Tests.ElasticBufferWb where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)

import Bittide.DoubleBufferedRam (ContentType (Vec), InitialContent (NonReloadable))
import Bittide.ElasticBuffer
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.SharedTypes (withBittideByteOrder)
import Bittide.Wishbone
import Clash.Class.BitPackC
import GHC.Stack (HasCallStack)
import Project.FilePath
import Protocols
import Protocols.Idle
import Protocols.MemoryMap
import System.Environment (lookupEnv)
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import VexRiscv (DumpVcd (..))

-- | Strip "SimOnly" constructor
unSimOnly :: SimOnly a -> a
unSimOnly (SimOnly x) = x

dutMM :: (HasCallStack) => Protocols.MemoryMap.MemoryMap
dutMM =
  (\(SimOnly mm, _) -> mm)
    $ withClockResetEnable @XilinxSystem clockGen resetGen enableGen
    $ toSignals dut ((), pure $ deepErrorX "uart_bwd")

{- | A simulation-only instance containing VexRisc with UART and an elastic buffer
controlled via Wishbone. The processor runs the `elastic_buffer_wb_test` program
performing a series of tests on the elastic buffer via Wishbone and prints the
results over UART.
-}
dut ::
  (HasCallStack) =>
  Circuit
    (ConstBwd MM)
    ( Df XilinxSystem (BitVector 8) -- UART output
    )
dut = withBittideByteOrder $ withClockResetEnable clockGen (resetGenN d2) enableGen $ circuit $ \mm -> do
  (uartRx, jtagIdle) <- idleSource
  [ebWbBus, uartBus, (mmTime, timeBus)] <-
    processingElement @_ dumpVcd peConfig -< (mm, jtagIdle)
  (uartTx, _uartStatus) <- uartInterfaceWb d16 d2 uartBytes -< (uartBus, uartRx)

  -- Timer for delays instead of NOP loops
  _localCounter <- timeWb -< (mmTime, timeBus)

  -- Elastic buffer with Wishbone control and monitoring
  -- We use the same clock for both read and write domains for simplicity in testing
  _readData <-
    xilinxElasticBufferWb
      clockGen
      (resetGenN d2)
      d5
      clockGen
      (pure () :: Signal XilinxSystem ())
      -< ebWbBus

  idC -< uartTx
 where
  dumpVcd =
    unsafePerformIO $ do
      mVal <- lookupEnv "ELASTIC_BUFFER_WB_DUMP_VCD"
      case mVal of
        Just s -> pure (DumpVcd s)
        _ -> pure NoDumpVcd

  peConfig = unsafePerformIO $ do
    root <- findParentContaining "cabal.project"
    let
      elfDir = root </> firmwareBinariesDir "riscv32imc" Release
      elfPath = elfDir </> "elastic_buffer_wb_test"
    pure
      PeConfig
        { initI =
            NonReloadable @IMemWords
              $ Vec
              $ unsafePerformIO
              $ vecFromElfInstr BigEndian elfPath
        , initD =
            NonReloadable @DMemWords
              $ Vec
              $ unsafePerformIO
              $ vecFromElfData BigEndian elfPath
        , iBusTimeout = d0 -- No timeouts on the instruction bus
        , dBusTimeout = d0 -- No timeouts on the data bus
        , includeIlaWb = False
        }

type IMemWords = DivRU (64 * 1024) 4
type DMemWords = DivRU (32 * 1024) 4
