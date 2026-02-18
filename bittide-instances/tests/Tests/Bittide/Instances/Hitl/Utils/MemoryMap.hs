-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Tests.Bittide.Instances.Hitl.Utils.MemoryMap where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)

import Bittide.CaptureUgn
import Bittide.ElasticBuffer (ElasticBufferData (Data))
import Bittide.Instances.Hitl.Utils.MemoryMap (getPathAddress)
import Bittide.Instances.Tests.NestedInterconnect (nestedInterconnectMm)
import Bittide.ProcessingElement
import Bittide.SharedTypes (withBittideByteOrder)
import Control.Monad (forM_)
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
import qualified Data.List as L

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
      { depthI = SNat @1024
      , depthD = SNat @1024
      , initI = Nothing
      , initD = Nothing
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

-- | Convert an 'Either String a' to an 'Assertion'. Discards the 'a' value.
assertEither :: Either String a -> Assertion
assertEither (Right _) = pure ()
assertEither (Left msg) = error msg

-- | Convert an 'Either String a' to an 'IO a'. Throws an error on 'Left'.
expectRight :: (HasCallStack) => Either String a -> IO a
expectRight (Right v) = pure v
expectRight (Left msg) = error msg

-- | Convert an 'Either String a' to an 'Assertion'. Throws an error on 'Right'.
expectLeft :: Either String a -> Assertion
expectLeft (Left _) = pure ()
expectLeft (Right _) = error "Expected failure, but got a value"

case_check_all_reachable :: (HasCallStack) => Assertion
case_check_all_reachable = do
  forM_ [0 .. natToInteger @NumCaptureUgns - 1] $ \n -> do
    let devicePath = ["0", [i|CaptureUgn#{n}|]]
    assertEither $ getPathAddress @Integer exampleMm devicePath
    forM_ ["local_counter", "remote_counter", "has_captured"] $ \reg ->
      assertEither $ getPathAddress @Integer exampleMm (devicePath <> [reg])

  return ()

case_check_nonexistant_nonreachable :: (HasCallStack) => Assertion
case_check_nonexistant_nonreachable = do
  let
    assertNonReachable :: [String] -> IO ()
    assertNonReachable path = expectLeft $ getPathAddress @Integer exampleMm path

  assertNonReachable ["1"]
  assertNonReachable ["0", "Nonexistant"]
  assertNonReachable ["0", "CaptureUgn0", "nonexistant"]
  assertNonReachable ["0", "CaptureUgn200"]
  assertNonReachable ["0", "CaptureUgn200", "nonexistant"]

case_nested_interconnects :: (HasCallStack) => Assertion
case_nested_interconnects = do
  let
    peripheralRoots = [["0"], ["0", "3"], ["0", "3", "0"]]
    fmt root n = root <> ["somePeripheral" <> show n]
    peripheralPaths = L.concatMap (\root -> fmap (fmt root) [0 :: Int .. 2]) peripheralRoots
    allPaths = ["0", "Uart"] : peripheralPaths

    -- These addresses are extracted from the generated peripheral access code
    expectAddresses =
      (0x40000000 :: Int)
        : L.concat
          [ [0xA0000000, 0xC0000000, 0xE0000000] -- Unnested peripherals
          , [0x62000000, 0x64000000, 0x66000000] -- L1 peripherals
          , [0x60000000, 0x60200000, 0x60400000] -- L2 peripherals
          ]
  actualAddresses <- mapM (expectRight . getPathAddress nestedInterconnectMm) allPaths
  let checkPath (path, expect, actual) =
        assertEqual
          [i|Address for path #{L.intercalate " -> " path} does not match expected value|]
          expect
          actual
  forM_ (L.zip3 allPaths expectAddresses actualAddresses) checkPath

tests :: (HasCallStack) => TestTree
tests = $(testGroupGenerator)
