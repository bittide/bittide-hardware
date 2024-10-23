-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Wishbone.Axi where

-- Preludes
import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)

-- Local
import Bittide.Axi4
import Bittide.DoubleBufferedRam
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.SharedTypes
import Bittide.Wishbone
import Project.FilePath

-- Other
import Control.Monad (forM_)
import Data.Char
import Data.Maybe
import Data.Proxy
import Language.Haskell.TH
import Protocols
import Protocols.Axi4.Stream
import Protocols.Idle
import Protocols.Wishbone
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Text.Parsec
import Text.Parsec.String

-- Qualified
import qualified Protocols.Df as Df
import qualified Protocols.DfConv as DfConv

{- | Run the axi module self test with processingElement and inspect it's uart output.
The test returns names of tests and a boolean indicating if the test passed.
-}
case_axi_stream_rust_self_test :: Assertion
case_axi_stream_rust_self_test =
  -- Run the test with HUnit
  case parseTestResults simResult of
    Left errMsg -> assertFailure $ show errMsg <> "\n" <> simResult
    Right results -> do
      forM_ results $ \result -> assertResult result
 where
  assertResult (TestResult name (Just errMsg)) = assertFailure ("Test " <> name <> " failed with error \"" <> errMsg <> "\"")
  assertResult (TestResult _ Nothing) = return ()
  simResult = chr . fromIntegral <$> mapMaybe Df.dataToMaybe uartStream
  uartStream = sampleC def dut

{- | A simple instance containing just VexRisc and UART as peripheral.
Runs the `hello` binary from `firmware-binaries`.
-}
dut :: Circuit () (Df System (BitVector 8))
dut =
  withClockResetEnable clockGen resetGen enableGen
    $ circuit
    $ \_unit -> do
      (uartTx, jtag) <- idleSource -< ()
      [uartBus, axiTxBus, wbNull, axiRxBus] <- processingElement peConfig -< jtag
      wbAlwaysAck -< wbNull
      (uartRx, _uartStatus) <- uartInterfaceWb d2 d2 uartSim -< (uartBus, uartTx)
      _interrupts <- wbAxisRxBufferCircuit (SNat @128) -< (axiRxBus, axiStream)
      axiStream <-
        axiUserMapC (const False)
          <| DfConv.fifo axiProxy axiProxy (SNat @1024)
          <| axiPacking
          <| wbToAxiTx
          -< axiTxBus
      idC -< uartRx
 where
  axiProxy = Proxy @(Axi4Stream System ('Axi4StreamConfig 4 0 0) ())
  (iMem, dMem) =
    $( do
        root <- runIO $ findParentContaining "cabal.project"
        let
          elfDir = root </> firmwareBinariesDir "riscv32imc-unknown-none-elf" Release
          elfPath = elfDir </> "axi_stream_self_test"
        memBlobsFromElf BigEndian (Nothing, Nothing) elfPath Nothing
     )

  peConfig =
    PeConfig
      (0b000 :> 0b001 :> 0b010 :> 0b011 :> 0b100 :> 0b101 :> Nil)
      (Reloadable $ Blob iMem)
      (Reloadable $ Blob dMem)

data TestResult = TestResult String (Maybe String) deriving (Show, Eq)

wbAlwaysAck ::
  (NFDataX a) =>
  Circuit
    (Wishbone dom 'Standard addrW a)
    ()
wbAlwaysAck = Circuit (const (pure $ emptyWishboneS2M{acknowledge = True}, ()))

testResultParser :: Parser TestResult
testResultParser = do
  testName <- manyTill anyChar (try (string ": "))
  result <-
    choice
      [ string "None" >> return Nothing
      , Just <$> (string "Some(" *> manyTill anyChar (char ')'))
      ]
  _ <- endOfLine
  return $ TestResult testName result

testResultsParser :: Parser [TestResult]
testResultsParser = do
  _ <- string "Start axi self test" >> endOfLine
  manyTill testResultParser done
 where
  done = try (string "Done") >> endOfLine >> return ()

{- | Parse test results from the simulation output. See 'case_parseTestResults'
for example inputs.
-}
parseTestResults :: String -> Either ParseError [TestResult]
parseTestResults = parse testResultsParser ""

case_parseTestResults :: Assertion
case_parseTestResults = do
  Right [] @=? parseTestResults "Start axi self test\nDone\n"

  Right [TestResult "a" Nothing]
    @=? parseTestResults "Start axi self test\na: None\nDone\n"

  Right [TestResult "a" Nothing, TestResult "b" Nothing]
    @=? parseTestResults "Start axi self test\na: None\nb: None\nDone\n"

  Right [TestResult "a" (Just "1"), TestResult "b" Nothing]
    @=? parseTestResults "Start axi self test\na: Some(1)\nb: None\nDone\n"

tests :: TestTree
tests = $(testGroupGenerator)
