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
import Bittide.Instances.Hitl.Setup (FpgaCount, LinkCount, demoRigInfo, fpgaSetup)
import Bittide.Instances.Hitl.Utils.Driver
import Bittide.Instances.Hitl.Utils.Gdb (initGdb)
import Bittide.Instances.Hitl.Utils.MemoryMap (getPathAddress)
import Bittide.Instances.Hitl.Utils.OpenOcd (parseBootTapInfo, parseTapInfo)
import Bittide.Instances.Hitl.Utils.Picocom (initPicocom)
import Bittide.Instances.Hitl.Utils.Relabel (
  RelabelPlan (..),
  computeRelabel,
  hardwareUgnEdges,
  readCurrentTime,
  ugnEdges,
  writeCorrections,
  writeReleaseCycle,
 )
import Bittide.Instances.Hitl.Utils.Ugn (UgnEdge (..), indexToNodeId)
import Bittide.Instances.Hitl.Utils.UgnGrooming (safeMargin)
import Bittide.Instances.Hitl.Utils.Usb (resetUsbDeviceByLocation)
import Bittide.Instances.Hitl.Utils.Utils (dumpCcSamples)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently_, mapConcurrently_)
import Control.Concurrent.Async.Extra (zipWithConcurrently, zipWithConcurrently3_)
import Control.Monad (forM, forM_, unless)
import Control.Monad.IO.Class
import Data.Maybe (fromJust)
import Data.String.Interpolate (i)
import Data.Vector.Internal.Check (HasCallStack)
import Gdb (Gdb)
import Numeric (showHex)
import Project.Chan
import Project.FilePath
import Project.Handle (assertEither, expectRight)
import Protocols.MemoryMap (MemoryMap)
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
import qualified Gdb
import qualified System.Timeout.Extra as T

{- | Seconds of headroom between reading the current time and the reset-release /
schedule point. This must comfortably exceed the wall-clock time it takes to
write all per-node timestamps + configs over GDB (~10s for 8 nodes), otherwise
a node's reset can release before it is fully configured (see the post-write
check in 'driver').
-}
type StartDelay = 30 -- seconds

{- | The delay in clock cycles between 2 PEs which is not accounted for by the
`captureUgn` component. The wire demo schedule reasons about PE-to-PE timing, but
the UGN is measured between MU-side `sendUgn` and `captureUgn`, which sit further
along the data path than the PE taps. This delta backtracks the MU-measured UGN
to the PE-to-PE delay.
-}
internalDelay :: Int
internalDelay = -4

{- | Base cycle of the fixed schedule in the relabeled application-counter domain.
The application counter is 0 at the timed-reset release (tReset), so this small
base is a short head-start before the first node's PE fires.
-}
appScheduleBase :: Unsigned 64
appScheduleBase = 1000

{- | Frames of safety margin added to the golden UGNs to form @λ^safe@ (@λ^safe =
golden + ε@). Each boot is groomed up to @λ^safe@, so @ε@ is the number of frames the
elastic buffer inserts on top of matching the golden latency; it must cover the small
boot-to-boot latency drift yet stay within the buffer's safe range (~±12).
-}
marginFrames :: Signed 64
marginFrames = 5

{- | Golden UGN graph: a full set of raw per-node, per-link UGNs (@λ = local - remote@)
captured from a passing CI boot of this rig (FPGA-indexed, link-indexed like
'fpgaSetup'). This is the stored reference every boot is groomed onto.

Because it is a real boot of the same rig, @λ^safe - λ@ for any later boot is a pure
counter-offset coboundary (the physical link latencies — asymmetry and all — are
identical and cancel). Bellman-Ford ('computeRelabel') therefore removes the offset
exactly on every node and the residual frames are just 'marginFrames'.
-}
goldenUgns :: Vec FpgaCount (Vec LinkCount (Signed 64))
{- FOURMOLU_DISABLE -}
goldenUgns =
       ( 106204 :>  392191 :>   44456 :> -137763 :>  397168 :>   63442 :>  201544 :> Nil)
    :> ( 189459 :>  -95303 :> -339270 :>  195662 :> -138065 :> -157052 :> -201469 :> Nil)
    :> (-189373 :> -390884 :>    6246 :> -327481 :> -346468 :> -528687 :> -284719 :> Nil)
    :> (-106130 :>   95377 :>  -42724 :>  -61711 :> -243930 :>  290999 :>  284794 :> Nil)
    :> (  19024 :>  352751 :>  -44380 :>   61786 :>  346542 :>  157126 :> -182181 :> Nil)
    :> ( 534966 :>  201243 :>  339345 :>  137839 :>  244005 :>  528761 :>  182256 :> Nil)
    :> (-534895 :> -352673 :>   -6170 :> -195584 :> -397092 :> -290924 :> -333687 :> Nil)
    :> ( -18949 :> -201167 :>   42800 :>  327557 :>  138138 :>  -63366 :>  333764 :> Nil)
    :> Nil
{- FOURMOLU_ENABLE -}

{- | The stored @λ^safe@: the golden UGN graph plus 'marginFrames'. Every boot is
groomed onto this (see 'computeRelabel'), so the application sees the same per-edge UGNs
every run and the schedule is fixed.
-}
lambdaSafe :: [UgnEdge]
lambdaSafe = safeMargin marginFrames (ugnEdges goldenUgns)

{- | Collect the configuration for the 'wireDemoPeConfig' and the 'programmableMux' in
a single data structure for easier schedule generation.
-}
data NodeConfig linkCount = NodeConfig
  { firstBCycle :: Unsigned 64
  , readLink :: Maybe (Index linkCount)
  , writeLink :: Maybe (Index linkCount)
  }
  deriving (Show)

{- | Chain topology for the wire demo: for each node, the link index over which it
reads the previous node and the link index over which it writes the next node
(@Nothing@ at the two ends of the chain). Purely structural -- it depends only on
the FPGA link map, not on any measured timing. The per-node @first_b_cycle@ is set
separately from the relabeled schedule (see 'driver').
-}
chainTopology ::
  (KnownNat nodeCount, 1 <= nodeCount) =>
  Vec nodeCount (FpgaId, Vec (nodeCount - 1) (Index nodeCount)) ->
  -- | Per node: @(readLink, writeLink)@ as link indices into that node's links.
  Vec nodeCount (Maybe (Index (nodeCount - 1)), Maybe (Index (nodeCount - 1)))
chainTopology fpgaTable = imap node fpgaTable
 where
  node fpgaIndex (_, links) = (toLink <$> readFpgaIdx, toLink <$> writeFpgaIdx)
   where
    readFpgaIdx = if fpgaIndex == 0 then Nothing else Just (fpgaIndex - 1)
    writeFpgaIdx = if fpgaIndex == maxBound then Nothing else Just (fpgaIndex + 1)
    toLink target = fromJust (elemIndex target links)

{- | The fixed application-domain schedule, derived entirely from 'lambdaSafe' (and the
structural chain topology), hence constant across runs. After grooming, every node's
application counter is in the golden frame, so the PE chain fires at the golden boot's
schedule: per hop @k -> k+1@ the step is @1 + λ^safe_{k->k+1} + internalDelay@. Because
@λ^safe@ carries the golden boot's counter offsets, the running sum swings by those
offsets; it is shifted so the earliest @first_b_cycle@ is 'appScheduleBase' (all
non-negative). Boot offsets are removed by the per-node timed-reset release, not here.
-}
appSchedule :: Vec FpgaCount (NodeConfig (FpgaCount - 1))
appSchedule =
  zipWith
    (\firstBCycle (readLink, writeLink) -> NodeConfig{firstBCycle, readLink, writeLink})
    appFbcs
    (chainTopology fpgaSetup)
 where
  safeOf src dst = L.head [e.ugn | e <- lambdaSafe, e.srcNode == src, e.dstNode == dst]
  step k = 1 + toInteger (safeOf (indexToNodeId k) (indexToNodeId (k + 1))) + toInteger internalDelay
  steps = map step (init indicesI) :: Vec (FpgaCount - 1) Integer
  cumulative = scanl (+) 0 steps :: Vec FpgaCount Integer
  minCumulative = minimum cumulative
  appFbcs =
    map (\c -> checkedFromIntegral (toInteger appScheduleBase + c - minCumulative)) cumulative

{- | Read the hardware UGNs of the given device using the given GDB connection. Requires
the CPU to be halted.
-}
readHardwareUgns :: MemoryMap -> (HwTarget, DeviceInfo) -> Gdb -> IO [(Unsigned 64, Unsigned 64)]
readHardwareUgns mm (_, d) gdb = do
  let
    captureUgnPrefixed :: Int -> String -> [String]
    captureUgnPrefixed linkNr reg = ["0", "CaptureUgn" <> show linkNr, reg]

    readUgnMmio :: Int -> IO (Unsigned 64, Unsigned 64, Signed 32)
    readUgnMmio linkNr = do
      let getUgnRegister = expectRight . getPathAddress mm . captureUgnPrefixed linkNr
      counterAddresses <- mapM getUgnRegister ["local_counter", "remote_counter"]
      [localCounter, remoteCounter] <- mapM (Gdb.readLe gdb) counterAddresses
      delta <- Gdb.readLe gdb =<< getUgnRegister "elastic_buffer_delta"
      pure (localCounter, remoteCounter, delta)

    -- Adjust the local counter for the frames added/removed from the elastic
    -- buffer after capturing the UGN. Leaves the remote counter untouched.
    adjustLocalCounter :: (Unsigned 64, Unsigned 64, Signed 32) -> (Unsigned 64, Unsigned 64)
    adjustLocalCounter (localCounter, remoteCounter, delta) =
      (addSigned localCounter delta, remoteCounter)
     where
      addSigned :: Unsigned 64 -> Signed 32 -> Unsigned 64
      addSigned u s = checkedFromIntegral (toInteger u + toInteger s)

  liftIO $ putStrLn $ "Getting UGNs for device " <> d.deviceId
  ugnTriples <- mapM readUgnMmio [0 .. natToNum @(LinkCount - 1)]
  liftIO $ forM_ ugnTriples $ \triple -> putStrLn $ "Raw UGN triple: " <> show triple
  pure $ adjustLocalCounter <$> ugnTriples

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
  let getPeConfigRegister reg = expectRight $ getPathAddress MemoryMaps.managementUnit ["0", "WireDemoPeConfig", reg]
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
  let getMuxRegister reg = expectRight $ getPathAddress MemoryMaps.managementUnit ["0", "ProgrammableMux", reg]
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
  dnaBaseAddr <-
    expectRight $ getPathAddress @Integer MemoryMaps.managementUnit ["0", "Dna", "maybe_dna"]
  peConfigAddress <-
    expectRight $ getPathAddress MemoryMaps.managementUnit ["0", "WireDemoPeConfig", "written_data"]
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

  -- Reset USB adapter, see documentation of "Bittide.Instances.Hitl.Utils.Usb"
  liftIO $ forM_ targets $ \(_, d) -> resetUsbDeviceByLocation d.usbAdapterLocation

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
          $ zipWithConcurrently3_ (initGdb hitlDir "wire-demo-boot") bootGdbs bootTapInfos targets
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

      _bootTapInfos, managementUnitTapInfos, clockControlTapInfos :: [Ocd.TapInfo]
      (_bootTapInfos, managementUnitTapInfos, clockControlTapInfos)
        | all (== L.length expectedJtagIds) (L.length <$> allTapInfos)
        , [boots, mus, ccs] <- L.transpose allTapInfos =
            (boots, mus, ccs)
        | otherwise =
            error
              $ "Unexpected number of OpenOCD taps initialized. Expected: "
              <> show (L.length expectedJtagIds)
              <> ", but got: "
              <> show (L.length <$> allTapInfos)

    Gdb.withGdbs (L.length targets) $ \clockControlGdbs -> do
      liftIO
        $ zipWithConcurrently3_
          (initGdb hitlDir "wire-demo-clock-control")
          clockControlGdbs
          clockControlTapInfos
          targets
      liftIO $ mapConcurrently_ ((assertEither =<<) . Gdb.loadBinary) clockControlGdbs

      Gdb.withGdbs (L.length targets) $ \managementUnitGdbs -> do
        liftIO
          $ zipWithConcurrently3_
            (initGdb hitlDir "wire-demo-management-unit")
            managementUnitGdbs
            managementUnitTapInfos
            targets
        liftIO $ mapConcurrently_ ((assertEither =<<) . Gdb.loadBinary) managementUnitGdbs

        brackets picocomStarts (liftIO . snd) $ \(L.map fst -> picocoms) -> do
          let goDumpCcSamples = dumpCcSamples MemoryMaps.clockControl hitlDir (defCcConf (natToNum @FpgaCount)) clockControlGdbs
          liftIO $ mapConcurrently_ Gdb.continue clockControlGdbs
          liftIO $ mapConcurrently_ Gdb.continue managementUnitGdbs

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
          liftIO $ mapConcurrently_ Gdb.interrupt managementUnitGdbs
          ugnPairsTable <-
            liftIO $ zipWithConcurrently (readHardwareUgns MemoryMaps.managementUnit) targets managementUnitGdbs
          let
            ugnPairsTableV = fromJust . V.fromList $ fromJust . V.fromList <$> ugnPairsTable
          liftIO $ do
            putStrLn "Calculating IGNs for all targets"
            Calc.printAllIgns ugnPairsTableV fpgaSetup
            putStrLn "UGN pairs table:"
            mapM_ print ugnPairsTableV

          -- Relabel this boot onto the stored target latencies, using the reusable
          -- grooming components ("Bittide.Instances.Hitl.Utils.Relabel"): build UGN
          -- edges from the measured counters, groom them (Bellman-Ford) onto
          -- λ^safe = the target latency on both directions of every chain hop, and turn
          -- the result into per-node reset releases (which remove the boot-time counter
          -- offsets) and per-link frame corrections (which raise each link's latency to
          -- the target). The application then runs one FIXED schedule built from the
          -- target latencies -- identical every run.
          currentTime <-
            liftIO $ readCurrentTime MemoryMaps.managementUnit (L.head targets) (L.head managementUnitGdbs)
          let
            -- Shared reference instant for the relabel: node 0's current time plus a
            -- large constant. 'StartDelay' must exceed the GDB bookkeeping time, else a
            -- node's reset could release before it is fully configured.
            sharedBase = currentTime + natToNum @(PeriodToCycles GthTx (Seconds StartDelay))

            -- This boot's measured UGN edges for the FULL graph (every node, every
            -- link) -- the grooming is independent of the application and adjusts all
            -- elastic buffers.
            measuredEdges = hardwareUgnEdges ugnPairsTableV

          -- Groom this boot's measured UGNs onto the stored 'lambdaSafe' (Bellman-Ford):
          -- per-node reset offsets + per-link frame corrections for every edge.
          -- 'appSchedule' is fixed and derived from the same 'lambdaSafe'.
          RelabelPlan{resetOffsets, corrections = correctionsPerNode} <-
            case computeRelabel measuredEdges lambdaSafe of
              Left ns ->
                fail
                  $ "UGN grooming infeasible (UGNs changed too much); negative cycle through nodes: "
                  <> show ns
              Right plan -> pure plan

          let
            -- tReset_i = sharedBase + reset offset (the relabel that removes node i's
            -- boot-time counter offset; the application counter is 0 at release).
            tResets :: Vec FpgaCount (Unsigned 64)
            tResets =
              map (\off -> checkedFromIntegral (toInteger sharedBase + toInteger off)) resetOffsets
          liftIO $ do
            putStrLn
              [i|Shared reference base (node 0 now + #{natToNum @StartDelay :: Integer}s): #{sharedBase}|]
            putStrLn
              [i|Grooming onto stored lambda^safe (#{L.length lambdaSafe} edges, margin #{marginFrames})|]
            putStrLn "Per-node reset offset (relabel q, gauged to node 0):"
            mapM_ print (toList resetOffsets)
            putStrLn "Per-node tReset (= sharedBase + reset offset):"
            mapM_ print (toList tResets)
            putStrLn "Per-node frame corrections (per link):"
            mapM_ print (toList correctionsPerNode)
            putStrLn "Fixed app-domain schedule (relabeled domain, same every run):"
            mapM_ print appSchedule

          -- Step 3: write the fixed application configuration (in the relabeled domain).
          _ <- sequenceA $ L.zipWith3 writePeConfig targets managementUnitGdbs (toList appSchedule)
          _ <-
            sequenceA $ L.zipWith3 writeProgrammableMuxConfig targets managementUnitGdbs (toList appSchedule)

          -- Step 5: apply the elastic-buffer corrections BEFORE the application runs.
          -- The application is still held in reset (tReset not yet set), so resuming
          -- the MUs only runs the corrections firmware, which applies the frames
          -- (within a safe occupancy margin) and reports success; then halt again.
          liftIO $ do
            putStrLn "\n=== Applying elastic-buffer corrections ==="
            mapConcurrently_
              (\(gdb, corr) -> writeCorrections MemoryMaps.managementUnit gdb corr)
              (L.zip managementUnitGdbs (toList correctionsPerNode))
            mapConcurrently_ Gdb.continue managementUnitGdbs
            T.tryWithTimeoutOn
              T.PrintActionTime
              "Waiting for corrections to be applied"
              60_000_000
              goDumpCcSamples
              $ forConcurrently_ picocoms
              $ \pico -> waitForLine pico "[MU] Corrections applied successfully"
            mapConcurrently_ Gdb.interrupt managementUnitGdbs

          -- Step 6: set tReset so every node's application leaves reset at the relabeled
          -- moment (its application counter becomes 0 there).
          liftIO $ do
            putStrLn "\n=== Setting tReset ==="
            forM_ (L.zip managementUnitGdbs (toList tResets)) $ \(gdb, tReset) ->
              writeReleaseCycle MemoryMaps.managementUnit gdb tReset

          -- Safety check: every node must still be *before* its tReset. Writing the
          -- configs + corrections + tReset over GDB takes time; if it took longer than
          -- the 'StartDelay' margin, a node's local counter may already have passed its
          -- tReset, in which case its reset releases (and the programmable mux's
          -- equality trigger is missed) before the schedule is in effect.
          releaseInTime <-
            liftIO $ forM (L.zip3 targets managementUnitGdbs (toList tResets)) $ \(tgt@(_, d), gdb, tReset) -> do
              let did = d.deviceId
              now <- readCurrentTime MemoryMaps.managementUnit tgt gdb
              let inTime = now < tReset
              if inTime
                then
                  putStrLn
                    [i|  #{did}: ok (now=#{now} < tReset=#{tReset}, margin=#{toInteger tReset - toInteger now} cycles)|]
                else
                  putStrLn
                    [i|  [ERROR] #{did}: now=#{now} has already passed tReset=#{tReset} — setup took too long|]
              pure inTime
          unless (L.and releaseInTime)
            $ fail "Reset-release window missed: setup took longer than the StartDelay margin"

          -- Step 7: wait for the application to run, then read back and verify the data.
          liftIO $ do
            let delayMicros = natToNum @((StartDelay + 1) * 1_000_000)
            putStrLn [i|Sleeping for: #{delayMicros}μs ...|]
            threadDelay delayMicros
            newCurrentTime <-
              readCurrentTime MemoryMaps.managementUnit (L.head targets) (L.head managementUnitGdbs)
            putStrLn [i|Clock is now: #{newCurrentTime}|]

          let
            dnas = L.map (.dna) demoRigInfo
            expectedWrittenDatas = L.tail $ L.scanl xor 0 $ L.map resize dnas
          checks <-
            liftIO
              $ sequenceA
              $ L.zipWith4 verifyWrittenData targets managementUnitGdbs dnas expectedWrittenDatas

          liftIO goDumpCcSamples

          unless (L.and checks) $ fail "Some PE buffer did not write the expected data"

          pure ExitSuccess
