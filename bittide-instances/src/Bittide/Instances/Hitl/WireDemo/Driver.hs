-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}
-- TODO: Remove use of partial functions
{-# OPTIONS_GHC -Wno-x-partial #-}

module Bittide.Instances.Hitl.WireDemo.Driver where

import Clash.Prelude

import Bittide.ClockControl.Config (defCcConf)
import Bittide.Hitl
import Bittide.Instances.Domains (GthTx)
import Bittide.Instances.Hitl.Setup (FpgaCount, demoRigInfo, fpgaSetup)
import Bittide.Instances.Hitl.SwitchDemo.Driver (
  dumpCcSamples,
  initGdb,
  initPicocom,
  parseBootTapInfo,
  parseTapInfo,
  readCurrentTime,
  readHardwareUgns,
 )
import Bittide.Instances.Hitl.Utils.Driver
import Bittide.Instances.Hitl.Utils.MemoryMap (getPathAddress)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently_, mapConcurrently_)
import Control.Concurrent.Async.Extra (zipWithConcurrently, zipWithConcurrently3_)
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class
import Data.Bifunctor (Bifunctor (bimap))
import Data.Maybe (fromJust)
import Data.String.Interpolate (i)
import Data.Vector.Internal.Check (HasCallStack)
import Gdb (Gdb)
import Numeric (showHex)
import Project.Chan
import Project.FilePath
import Project.Handle (assertEither, expectRight)
import System.Exit
import System.FilePath
import Vivado.Tcl (HwTarget)
import Vivado.VivadoM (VivadoM)
import "bittide-extra" Control.Exception.Extra (brackets)

import qualified Bittide.Calculator as Calc
import qualified Bittide.Instances.Hitl.Utils.OpenOcd as Ocd
import qualified Bittide.Instances.Hitl.WireDemo.MemoryMaps as MemoryMaps
import qualified Clash.Sized.Vector as V
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Gdb
import qualified System.Timeout.Extra as T

type StartDelay = 10 -- seconds

{- | The delay in clock cycles between 2 PEs which is not accounted for by the
`captureUgn` component.

For the wire demo this delay is 1 cycle, caused by the 'dflipflop' at the output of
'core', which is _before_ the 'sendUgn' component and therefore not accounted for in the
captured UGN.
-}
type InternalDelay = 1

{- | Collect the configuration for the 'wireDemoPeConfig' and the 'programmableMux' in
a single data structure for easier schedule generation.
-}
data NodeConfig linkCount = NodeConfig
  { firstBCycle :: Unsigned 64
  , readLink :: Maybe (Index linkCount)
  , writeLink :: Maybe (Index linkCount)
  }
  deriving (Show)

{- | Generate a schedule for the wire demo, which consists of a configuration for the PE
and the programmable mux for each node.

First generates an 'absolute' schedule with only a few rules:
  - 'firstBCycle' is always 1 cycle after the previous node's 'firstBCycle'
  - 'readLink' is always the previous node, except for the first node
  - 'writeLink' is always the next node, except for the last node

In this absolute schedule the links are FPGA indexed (not link indexed) and all
`firstBCycle`s are relative to the first node's `firstBCycle`.

Next, this absolute schedule is converted to a 'relative' schedule by:
  - Converting 'firstBCycle' to account for the counter mapping between the nodes
  - Converting FPGA indexed links to link indices based on the FPGA setup
This process should be done in a chain-like manner, similar to the calculator for the
switch demo.
-}
generateSchedule ::
  forall nodeCount.
  ( KnownNat nodeCount
  , 1 <= nodeCount
  ) =>
  Vec nodeCount (FpgaId, Vec (nodeCount - 1) (Index nodeCount)) ->
  Vec nodeCount (Vec (nodeCount - 1) (Unsigned 64, Unsigned 64)) ->
  Unsigned 64 ->
  Vec nodeCount (NodeConfig (nodeCount - 1))
generateSchedule fpgaTable ugnParts startCycle = genConfig <$> iterateI nextState (0, startCycle)
 where
  -- Map a clock cycle in the source domain to the destination domain. Goes through integer
  -- because the mapping can be negative. If the result would be negative, return maxBound
  -- to indicate that the event will not happen within the timespan of this test.
  mapCycle :: Unsigned 64 -> Index nodeCount -> Index nodeCount -> Unsigned 64
  mapCycle srcCycle src dst = if dstCycleI < 0 then maxBound else fromInteger dstCycleI
   where
    dstCycleI = srcCycleI + counterMap !! src Map.! dst
    srcCycleI = toInteger srcCycle

    ugnPartsI = map (map (bimap toInteger toInteger)) ugnParts
    counterMap = Calc.toCounterMap (SNat @InternalDelay) (Calc.toFpgaIndexed fpgaTable ugnPartsI)

  nextState (fpgaIndex, firstBCycle) = (fpgaIndex + 1, nextFirstBCycle)
   where
    nextFirstBCycle = mapCycle (firstBCycle + 1) fpgaIndex (fpgaIndex + 1)

  genConfig (fpgaIndex, firstBCycle) =
    NodeConfig
      { firstBCycle
      , readLink = toLinkIndex fpgaIndex <$> readFpgaIdx
      , writeLink = toLinkIndex fpgaIndex <$> writeFpgaIdx
      }
   where
    readFpgaIdx = if fpgaIndex == 0 then Nothing else Just (fpgaIndex - 1)
    writeFpgaIdx = if fpgaIndex == maxBound then Nothing else Just (fpgaIndex + 1)

    toLinkIndex currentFpga targetFpga =
      let links = snd (fpgaTable !! currentFpga)
       in fromJust $ elemIndex targetFpga links

writePeConfig ::
  ( HasCallStack
  , KnownNat linkCount
  , 1 <= linkCount
  ) =>
  (HwTarget, DeviceInfo) ->
  Gdb ->
  NodeConfig linkCount ->
  VivadoM ()
writePeConfig (_, d) gdb nodeConfig = do
  let getPeConfigRegister reg = expectRight $ getPathAddress MemoryMaps.mu ["0", "WireDemoPeConfig", reg]
  liftIO $ do
    putStrLn $ "Writing PE config for target " <> d.deviceId
    addresses <- mapM getPeConfigRegister ["read_link", "write_link"]
    let values = [nodeConfig.readLink, nodeConfig.writeLink]
    _ <- sequence $ L.zipWith (Gdb.writeLe gdb) addresses values
    pure ()

writeProgrammableMuxConfig ::
  (HasCallStack, KnownNat linkCount) =>
  (HwTarget, DeviceInfo) ->
  Gdb ->
  NodeConfig linkCount ->
  VivadoM ()
writeProgrammableMuxConfig (_, d) gdb nodeConfig = do
  let getMuxRegister reg = expectRight $ getPathAddress MemoryMaps.mu ["0", "ProgrammableMux", reg]
  liftIO $ do
    putStrLn $ "Writing programmable mux config for target " <> d.deviceId
    firstBCycleAddress <- getMuxRegister "first_b_cycle"
    armAddress <- getMuxRegister "arm"
    Gdb.writeLe gdb firstBCycleAddress nodeConfig.firstBCycle
    Gdb.writeLe gdb armAddress True
    pure ()

{- | Check that a node has the expected DNA and that the PE has written the expected data
to the next node.
-}
verifyWrittenData ::
  (HasCallStack) =>
  (HwTarget, DeviceInfo) ->
  Gdb ->
  BitVector 96 ->
  BitVector 64 ->
  IO Bool
verifyWrittenData (_, d) gdb expectedDna expectedData = do
  putStrLn $ "Reading PE written data for target " <> d.deviceId
  dnaBaseAddr <- expectRight $ getPathAddress @Integer MemoryMaps.mu ["0", "Dna", "maybe_dna"]
  peConfigAddress <-
    expectRight $ getPathAddress MemoryMaps.mu ["0", "WireDemoPeConfig", "written_data"]
  maybeDna <- Gdb.readLe @(Maybe (BitVector 96)) gdb dnaBaseAddr
  writtenData <- Gdb.readLe gdb peConfigAddress

  let dna = fromJust maybeDna
  putStrLn $ "  Expected device DNA:      " <> showHex expectedDna ""
  putStrLn $ "  Actual device DNA:        " <> showHex dna ""
  putStrLn $ "  Expected PE written data: " <> showHex expectedData ""
  putStrLn $ "  Actual PE written data:   " <> showHex writtenData ""

  pure $ dna == expectedDna && writtenData == expectedData

driver ::
  (HasCallStack) =>
  String ->
  [(HwTarget, DeviceInfo)] ->
  VivadoM ExitCode
driver testName targets = do
  liftIO
    . putStrLn
    $ "Running driver function for targets "
    <> show ((\(_, info) -> info.deviceId) <$> targets)

  projectDir <- liftIO $ findParentContaining "cabal.project"
  let hitlDir = projectDir </> "_build/hitl" </> testName

  forM_ targets (assertProbe "probe_test_start")

  let
    -- BOOT / MU / CC IDs
    expectedJtagIds = [0x0514C001, 0x1514C001, 0x2514C001]
    toInitArgs (_, deviceInfo) targetIndex =
      Ocd.InitOpenOcdArgs{deviceInfo, expectedJtagIds, hitlDir, targetIndex}
    initArgs = L.zipWith toInitArgs targets [0 ..]
    optionalBootInitArgs = L.repeat def{Ocd.logPrefix = "boot-", Ocd.initTcl = "vexriscv_boot_init.tcl"}
    openOcdBootStarts = liftIO <$> L.zipWith Ocd.initOpenOcd initArgs optionalBootInitArgs

  let picocomStarts = liftIO <$> L.zipWith (initPicocom hitlDir) targets [0 ..]
  brackets picocomStarts (liftIO . snd) $ \(L.map fst -> picocoms) -> do
    -- Start OpenOCD that will program the boot CPU
    brackets openOcdBootStarts (liftIO . (.cleanup)) $ \initOcdsData -> do
      let bootTapInfos = parseBootTapInfo <$> initOcdsData

      Gdb.withGdbs (L.length targets) $ \bootGdbs -> do
        liftIO
          $ zipWithConcurrently3_ (initGdb hitlDir "switch-demo1-boot") bootGdbs bootTapInfos targets
        liftIO $ mapConcurrently_ ((assertEither =<<) . Gdb.loadBinary) bootGdbs
        liftIO $ mapConcurrently_ Gdb.continue bootGdbs
        liftIO
          $ T.tryWithTimeout T.PrintActionTime "Waiting for done" 60_000_000
          $ forConcurrently_ picocoms
          $ \pico ->
            waitForLine pico "[BT] Going into infinite loop.."

  let
    optionalInitArgs = L.repeat def
    openOcdStarts = liftIO <$> L.zipWith Ocd.initOpenOcd initArgs optionalInitArgs

  -- Start OpenOCD instances for all CPUs
  brackets openOcdStarts (liftIO . (.cleanup)) $ \initOcdsData -> do
    let
      allTapInfos = parseTapInfo expectedJtagIds <$> initOcdsData

      _bootTapInfos, muTapInfos, ccTapInfos :: [Ocd.TapInfo]
      (_bootTapInfos, muTapInfos, ccTapInfos)
        | all (== L.length expectedJtagIds) (L.length <$> allTapInfos)
        , [boots, mus, ccs] <- L.transpose allTapInfos =
            (boots, mus, ccs)
        | otherwise =
            error
              $ "Unexpected number of OpenOCD taps initialized. Expected: "
              <> show (L.length expectedJtagIds)
              <> ", but got: "
              <> show (L.length <$> allTapInfos)

    Gdb.withGdbs (L.length targets) $ \ccGdbs -> do
      liftIO $ zipWithConcurrently3_ (initGdb hitlDir "clock-control") ccGdbs ccTapInfos targets
      liftIO $ mapConcurrently_ ((assertEither =<<) . Gdb.loadBinary) ccGdbs

      Gdb.withGdbs (L.length targets) $ \muGdbs -> do
        liftIO $ zipWithConcurrently3_ (initGdb hitlDir "wire-demo-mu") muGdbs muTapInfos targets
        liftIO $ mapConcurrently_ ((assertEither =<<) . Gdb.loadBinary) muGdbs

        brackets picocomStarts (liftIO . snd) $ \(L.map fst -> picocoms) -> do
          let goDumpCcSamples = dumpCcSamples hitlDir (defCcConf (natToNum @FpgaCount)) ccGdbs
          liftIO $ mapConcurrently_ Gdb.continue ccGdbs
          liftIO $ mapConcurrently_ Gdb.continue muGdbs

          liftIO
            $ T.tryWithTimeoutOn
              T.PrintActionTime
              "Waiting for captured UGNs"
              60_000_000
              goDumpCcSamples
            $ forConcurrently_ picocoms
            $ \pico ->
              waitForLine pico "[MU] Printed all hardware UGNs"

          liftIO $ putStrLn "Getting UGNs for all targets"
          liftIO $ mapConcurrently_ Gdb.interrupt muGdbs
          ugnPairsTable <- liftIO $ zipWithConcurrently (readHardwareUgns MemoryMaps.mu) targets muGdbs
          let
            ugnPairsTableV = fromJust . V.fromList $ fromJust . V.fromList <$> ugnPairsTable
          liftIO $ do
            putStrLn "Calculating IGNs for all targets"
            Calc.printAllIgns ugnPairsTableV fpgaSetup
            putStrLn "UGN pairs table:"
            mapM_ print ugnPairsTableV

          -- Calculate schedule and write configurations
          currentTime <- liftIO $ readCurrentTime MemoryMaps.mu (L.head targets) (L.head muGdbs)
          let
            startOffset = currentTime + natToNum @(PeriodToCycles GthTx (Seconds StartDelay))
            schedule = generateSchedule fpgaSetup ugnPairsTableV startOffset
          liftIO $ do
            putStrLn "Generated schedule:"
            mapM_ print schedule
          liftIO $ putStrLn [i|Starting clock cycle: #{startOffset}|]
          _ <- sequenceA $ L.zipWith3 writePeConfig targets muGdbs (toList schedule)
          _ <- sequenceA $ L.zipWith3 writeProgrammableMuxConfig targets muGdbs (toList schedule)

          -- Wait for test completion
          liftIO $ do
            let delayMicros = natToNum @((StartDelay + 1) * 1_000_000)
            putStrLn [i|Sleeping for: #{delayMicros}μs ...|]
            threadDelay delayMicros
            newCurrentTime <- readCurrentTime MemoryMaps.mu (L.head targets) (L.head muGdbs)
            putStrLn [i|Clock is now: #{newCurrentTime}|]

          -- Verify test results
          let
            dnas = L.map (.dna) demoRigInfo
            expectedWrittenDatas = L.tail $ L.scanl xor 0 $ L.map resize dnas
          checks <- liftIO $ sequenceA $ L.zipWith4 verifyWrittenData targets muGdbs dnas expectedWrittenDatas

          liftIO goDumpCcSamples

          unless (L.and checks) $ fail "Some PE buffer did not write the expected data"

          pure ExitSuccess
