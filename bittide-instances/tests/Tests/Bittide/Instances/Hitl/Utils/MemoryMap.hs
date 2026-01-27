-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Tests.Bittide.Instances.Hitl.Utils.MemoryMap where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)

import Bittide.CaptureUgn
import Bittide.DoubleBufferedRam
import Bittide.ElasticBuffer (ElasticBufferData (Data))
import Bittide.Instances.Hitl.Utils.MemoryMap (getPathAddress)
import Bittide.ProcessingElement
import Bittide.SharedTypes (withBittideByteOrder)
import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.List (intercalate)
import Data.String.Interpolate (i)
import Protocols
import Protocols.Idle
import Protocols.MemoryMap
import Protocols.Vec (vecCircuits)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import VexRiscv (DumpVcd (NoDumpVcd))

import qualified Bittide.Cpus.Riscv32imc as Riscv32imc

type NumCaptureUgns = 4

exampleDevice ::
  (HasCallStack) =>
  Circuit (ToConstBwd Mm) ()
exampleDevice =
  withClockResetEnable @System clockGen resetGen enableGen
    $ withBittideByteOrder
    $ circuit
    $ \mm -> do
      jtag <- idleSource
      peWbs <- processingElement NoDumpVcd peConfig -< (mm, jtag)
      _ugns <- vecCircuits (captureUgn (pure 0) <$> (repeat (pure (Data Nothing)))) -< peWbs
      guh <- idleSource
      idC -< guh
 where
  peConfig :: PeConfig (PeInternalBusses + NumCaptureUgns)
  peConfig =
    PeConfig
      { initI = Undefined @1024
      , initD = Undefined @1024
      , iBusTimeout = SNat @0
      , dBusTimeout = SNat @0
      , includeIlaWb = False
      , cpu = Riscv32imc.vexRiscv0
      }

exampleMm :: MemoryMap
exampleMm = mm
 where
  Circuit circuitFn = exampleDevice
  (SimOnly mm, _) = circuitFn ((), ())

case_check_all_reachable :: (HasCallStack) => Assertion
case_check_all_reachable = do
  forM_ [0 .. natToInteger @NumCaptureUgns - 1] $ \n -> do
    let
      name = [i|CaptureUgn#{n}|]
      getDeviceBaseTest = do
        result <- try @SomeException $ return $! getPathAddress @Integer exampleMm ["0", name]
        case result of
          Right _ -> return ()
          Left e -> error [i|Error getting base address for #{name}: #{e}|]
    getDeviceBaseTest
    forM_ ["local_counter", "remote_counter", "has_captured"] $ \reg -> do
      result <- try @SomeException $ return $ getPathAddress @Integer exampleMm ["0", name, reg]
      case result of
        Right _ -> return ()
        Left e -> error [i|Error getting address for #{name}/#{reg}: #{e}|]

  return ()

case_check_nonexistant_nonreachable :: (HasCallStack) => Assertion
case_check_nonexistant_nonreachable = do
  let
    assertNonReachable :: [String] -> IO ()
    assertNonReachable path = do
      result <- try @SomeException $ return $! getPathAddress @Integer exampleMm path
      case result of
        Right _ -> error [i|Getting base address for `/#{intercalate "/" path}` should fail|]
        Left _ -> return ()

  assertNonReachable ["1"]
  assertNonReachable ["0", "Nonexistant"]
  assertNonReachable ["0", "CaptureUgn0", "nonexistant"]
  assertNonReachable ["0", "CaptureUgn200"]
  assertNonReachable ["0", "CaptureUgn200", "nonexistant"]

tests :: (HasCallStack) => TestTree
tests = $(testGroupGenerator)
