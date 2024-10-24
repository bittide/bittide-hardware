-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Wishbone.DnaPortE2 where

import Clash.Explicit.Prelude
import Clash.Prelude (HiddenClockResetEnable, withClockResetEnable)
import qualified Prelude as P

import Clash.Cores.Xilinx.Unisim.DnaPortE2
import Data.Char
import Data.Maybe
import Language.Haskell.TH
import Numeric
import Project.FilePath
import Protocols
import Protocols.Idle
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Bittide.DoubleBufferedRam
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.SharedTypes
import Bittide.Wishbone

import qualified Protocols.Df as Df

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
  clk = clockGen
  rst = resetGen
  simResult = chr . fromIntegral <$> mapMaybe Df.dataToMaybe uartStream
  uartStream = sampleC def $ withClockResetEnable clk rst enableGen $ dut @System <| idleSource

{- | A simple instance containing just VexRisc with UART and the DNA peripheral which
runs the `dna_port_e2_test` binary from `firmware-binaries`.
-}
dut ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  Circuit (Df dom (BitVector 8)) (Df dom (BitVector 8))
dut = circuit $ \uartRx -> do
  jtag <- idleSource -< ()
  [uartBus, dnaWb] <- processingElement @dom peConfig -< jtag
  (uartTx, _uartStatus) <- uartInterfaceWb d2 d2 uartSim -< (uartBus, uartRx)
  readDnaPortE2Wb simDna2 -< dnaWb
  idC -< uartTx
 where
  (iMem, dMem) =
    $( do
        root <- runIO $ findParentContaining "cabal.project"
        let
          elfDir = root </> firmwareBinariesDir "riscv32imc-unknown-none-elf" Release
          elfPath = elfDir </> "dna_port_e2_test"

        memBlobsFromElf BigEndian (Nothing, Nothing) elfPath Nothing
     )

  peConfig =
    PeConfig
      (0b00 :> 0b01 :> 0b10 :> 0b11 :> Nil)
      (Reloadable $ Blob iMem)
      (Reloadable $ Blob dMem)

parseResult :: String -> BitVector 96
parseResult = pack . (read :: String -> Unsigned 96) . P.head . lines

tests :: TestTree
tests = $(testGroupGenerator)
