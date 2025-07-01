-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
-- Don't warn about partial functions: this is a test, so we'll see it fail.
{-# OPTIONS_GHC -Wno-x-partial #-}

module Wishbone.DnaPortE2 where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClockResetEnable, withClockResetEnable)
import qualified Prelude as P

import Clash.Class.BitPackC (ByteOrder (BigEndian))
import Clash.Cores.Xilinx.Unisim.DnaPortE2
import Data.Char
import Data.Maybe
import Numeric
import Project.FilePath
import Protocols
import Protocols.Idle
import Protocols.MemoryMap
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import VexRiscv (DumpVcd (NoDumpVcd))

import Bittide.DoubleBufferedRam
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.Wishbone

sim :: IO ()
sim = putStr simResult

simResult :: String
simResult = chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream = sampleC def $ withClockResetEnable clockGen resetGen enableGen $ dut @System

-- | Test whether we can read the DNA from the DNA port peripheral.
case_dna_port_self_test :: Assertion
case_dna_port_self_test = assertBool msg (receivedDna == simDna2)
 where
  msg =
    "Received dna "
      <> showHex receivedDna ""
      <> " not equal to expected dna "
      <> showHex simDna2 ""
  receivedDna = parseResult simResult

{- | A simple instance containing just VexRisc with UART and the DNA peripheral which
runs the `dna_port_e2_test` binary from `firmware-binaries`.
-}
dut ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  Circuit () (Df dom (BitVector 8))
dut = circuit $ \_unit -> do
  (uartRx, jtag) <- idleSource
  [(prefixUart, (mmUart, uartBus)), (prefixDna, dnaBus)] <-
    processingElement @dom NoDumpVcd peConfig -< (mm, jtag)
  (uartTx, _uartStatus) <- uartInterfaceWb d2 d2 uartSim -< (mmUart, (uartBus, uartRx))
  mm <- ignoreMM
  constBwd 0b10 -< prefixUart
  _dna <- readDnaPortE2Wb simDna2 -< dnaBus
  constBwd 0b11 -< prefixDna
  idC -< uartTx
 where
  peConfig = unsafePerformIO $ do
    root <- findParentContaining "cabal.project"
    let elfPath = root </> firmwareBinariesDir "riscv32imc" Release </> "dna_port_e2_test"
    (iMem, dMem) <- vecsFromElf @IMemWords @DMemWords BigEndian elfPath Nothing
    pure
      PeConfig
        { initI = Reloadable (Vec iMem)
        , prefixI = 0b00
        , initD = Reloadable (Vec dMem)
        , prefixD = 0b01
        , iBusTimeout = d0 -- No timeouts on the instruction bus
        , dBusTimeout = d0 -- No timeouts on the data bus
        , includeIlaWb = False
        }
{-# NOINLINE dut #-}

type IMemWords = DivRU (64 * 1024) 4
type DMemWords = DivRU (32 * 1024) 4

parseResult :: String -> BitVector 96
parseResult = pack . (read :: String -> Unsigned 96) . P.head . lines

tests :: TestTree
tests = $(testGroupGenerator)
