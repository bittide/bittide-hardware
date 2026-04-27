-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
-- Don't warn about partial functions: this is a test, so we'll see it fail.
{-# OPTIONS_GHC -Wno-x-partial #-}

module Wishbone.DnaPortE2 where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClockResetEnable, withClockResetEnable)

import Clash.Cores.Xilinx.Unisim.DnaPortE2
import Data.Char
import Data.Maybe
import Numeric
import Project.FilePath
import Protocols
import Protocols.Idle
import Protocols.MemoryMap
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import VexRiscv (DumpVcd (NoDumpVcd))

import Bittide.Instances.Common (PeConfigElfSource (NameOnly), peConfigFromElf)
import Bittide.ProcessingElement
import Bittide.SharedTypes (withLittleEndian)
import Bittide.Wishbone

import qualified Bittide.Cpus.Riscv32imc as Riscv32imc
import qualified Prelude as P

sim :: IO ()
sim = do
  peConfig <- peConfigSim
  putStr $ simResult peConfig

simResult :: PeConfig 4 -> String
simResult peConfig = chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream =
    sampleC def
      $ withClockResetEnable clockGen (resetGenN d2) enableGen
      $ dut @System peConfig

-- | Test whether we can read the DNA from the DNA port peripheral.
case_dna_port_self_test :: Assertion
case_dna_port_self_test = do
  peConfig <- peConfigSim
  let
    msg =
      "Received dna "
        <> showHex receivedDna ""
        <> " not equal to expected dna "
        <> showHex simDna2 ""
    receivedDna = parseResult $ simResult peConfig
  assertBool msg (receivedDna == simDna2)

{- | A simple instance containing just VexRisc with UART and the DNA peripheral which
runs the `dna_port_e2_test` binary from `firmware-binaries`.
-}
dut ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  PeConfig 4 ->
  Circuit () (Df dom (BitVector 8))
dut peConfig = withLittleEndian $ circuit $ \_unit -> do
  (uartRx, jtag) <- idleSource
  [uartBus, dnaBus] <-
    processingElement @dom NoDumpVcd peConfig -< (mm, jtag)
  (uartTx, _uartStatus) <- uartInterfaceWb d2 d2 uartBytes -< (uartBus, uartRx)
  mm <- ignoreMM
  _dna <- readDnaPortE2Wb simDna2 -< dnaBus
  idC -< uartTx
{-# OPAQUE dut #-}

type IMemWords = DivRU (4 * 1024) 4
type DMemWords = DivRU (4 * 1024) 4

peConfigSim :: IO (PeConfig 4)
peConfigSim =
  peConfigFromElf
    (SNat @IMemWords)
    (SNat @DMemWords)
    (NameOnly "dna_port_e2_test")
    Release
    d0
    d0
    False
    Riscv32imc.vexRiscv0

parseResult :: String -> BitVector 96
parseResult = pack . (read :: String -> Unsigned 96) . P.head . lines

tests :: TestTree
tests = $(testGroupGenerator)
