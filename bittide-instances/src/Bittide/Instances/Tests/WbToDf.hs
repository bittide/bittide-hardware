-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.Tests.WbToDf where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)

import Bittide.Cpus.Riscv32imc (vexRiscv0)
import Bittide.Df
import Bittide.Instances.Common (
  PeConfigElfSource (NameOnly),
  dumpVcdFromEnvVar,
  emptyPeConfig,
  peConfigFromElf,
 )
import Bittide.ProcessingElement
import Bittide.SharedTypes (withLittleEndian)
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
dutMM =
  getMMAny $ dut NoDumpVcd $ emptyPeConfig (SNat @IMemWords) (SNat @DMemWords) d0 d0 False vexRiscv0

{- | A simulation-only instance containing VexRisc with UART, a register to hold
a vector of input values and a wbToDf component to send out values over `Df`.
The processor runs the `wb_to_df_test` program which copies values from the
register to the `Df` output and also sends some debug messages over UART.
-}
dut ::
  (HasCallStack) =>
  DumpVcd ->
  PeConfig 5 ->
  Circuit (ToConstBwd Mm, ()) (Df System SomeAdt, Df System (BitVector 8))
dut dumpVcd peConfig =
  withLittleEndian
    $ withClockResetEnable clockGen (resetGenN d2) enableGen
    $ circuit
    $ \(mm, _unit) -> do
      (uartRx, jtagIdle) <- idleSource
      [srcBus, dfBus, uartBus] <-
        processingElement @_ dumpVcd peConfig -< (mm, jtagIdle)
      (uartTx, _uartStatus) <- uartInterfaceWb d16 d2 uartBytes -< (uartBus, uartRx)

      [refWb] <- deviceWbI (deviceConfig "WbToDfReference") -< srcBus
      registerWbI_ refCfg testValue -< (refWb, Fwd (pure Nothing))

      df <- wbToDf "WbToDfTest" -< dfBus
      idC -< (df, uartTx)
 where
  refCfg =
    (registerConfig "value")
      { description = "Reference memory for WbToDfTest"
      , access = ReadOnly
      }

type IMemWords = DivRU (1 * 1024) 4
type DMemWords = DivRU (4 * 1024) 4

getDumpVcd :: IO DumpVcd
getDumpVcd = dumpVcdFromEnvVar "WBTODF_DUMP_VCD"

peConfigSim :: IO (PeConfig 5)
peConfigSim =
  peConfigFromElf
    (SNat @IMemWords)
    (SNat @DMemWords)
    (NameOnly "wb_to_df_test")
    Release
    d0
    d0
    False
    vexRiscv0
