-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.Tests.WbToDf where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)

import Bittide.Cpus.Riscv32 (riscv32Imc0)
import Bittide.Df
import Bittide.DoubleBufferedRam
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.SharedTypes (withBittideByteOrder)
import Bittide.Wishbone
import Clash.Class.BitPackC
import Control.DeepSeq (NFData)
import GHC.Stack (HasCallStack)
import Project.FilePath
import Protocols
import Protocols.Idle
import Protocols.MemoryMap
import Protocols.MemoryMap.Registers.WishboneStandard
import Protocols.MemoryMap.TypeDescription.TH
import System.Environment (lookupEnv)
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import VexRiscv (DumpVcd (..))

data SomeAdt
  = ConA
  | ConB (Unsigned 40)
  | ConC (BitVector 16) (BitVector 20)
  | ConD (Vec 3 (BitVector 8))
  deriving (Generic, NFDataX, ShowX, Show, BitPackC, BitPack, Eq, NFData)
deriveTypeDescription ''SomeAdt

testValue :: Vec 4 SomeAdt
testValue =
  ConA
    :> ConB 0xDEADABBABAAB1234
    :> ConC 0xDEAD 0xBEEF
    :> ConD (0xA :> 0xB :> 0xC :> Nil)
    :> Nil

-- | Strip "SimOnly" constructor
unSimOnly :: SimOnly a -> a
unSimOnly (SimOnly x) = x

dutMM :: (HasCallStack) => Protocols.MemoryMap.MemoryMap
dutMM = unSimOnly $ fst (toSignals dut (deepErrorX "memoryMap"))

{- | A simulation-only instance containing VexRisc with UART, a register to hold
a vector of input values and a wbToDf component to send out values over `Df`.
The processor runs the `wb_to_df_test` program which copies values from the
register to the `Df` output and also sends some debug messages over UART.
-}
dut ::
  (HasCallStack) =>
  Circuit (ToConstBwd Mm) (Df System SomeAdt, Df System (BitVector 8))
dut = withBittideByteOrder $ withClockResetEnable clockGen (resetGenN d2) enableGen $ circuit $ \mm -> do
  (uartRx, jtagIdle) <- idleSource
  [srcBus, dfBus, uartBus] <-
    processingElement @_ dumpVcd peConfig -< (mm, jtagIdle)
  (uartTx, _uartStatus) <- uartInterfaceWb d16 d2 uartBytes -< (uartBus, uartRx)

  [refWb] <- deviceWb "WbToDfReference" -< srcBus
  registerWbI_ refCfg testValue -< (refWb, Fwd (pure Nothing))

  df <- wbToDf "WbToDfTest" -< dfBus
  idC -< (df, uartTx)
 where
  refCfg =
    (registerConfig "value")
      { description = "Reference memory for WbToDfTest"
      , access = ReadOnly
      }

  dumpVcd =
    unsafePerformIO $ do
      mVal <- lookupEnv "WBTODF_DUMP_VCD"
      case mVal of
        Just s -> pure (DumpVcd s)
        _ -> pure NoDumpVcd

  peConfig = unsafePerformIO $ do
    root <- findParentContaining "cabal.project"
    let
      elfDir = root </> firmwareBinariesDir "riscv32imc" Release
      elfPath = elfDir </> "wb_to_df_test"
    pure
      PeConfig
        { cpu = riscv32Imc0
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

type IMemWords = DivRU (64 * 1024) 4
type DMemWords = DivRU (32 * 1024) 4
