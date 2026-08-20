-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}
-- TODO: Remove use of partial functions
{-# OPTIONS_GHC -Wno-x-partial #-}

{- | Host driver for the decode demo: runs the same ring all-reduce workload
under every transport discipline, back to back, on one bitstream:

1. __C__ (software async): management-unit rendezvous over ring buffers.
2. __A′__ (software scheduled): the same engine fired by timer deadlines.
3. The programmable mux is armed — one-shot, so the management-unit path is
   gone from here on.
4. __A__ (scheduled): the hardware processing element on the calendar.
5. __B__ (hardware async): the same datapath behind the credit link,
   cut-through.
6. __B_sf__: credit link with store-and-forward relays.

Latency histograms land in @_build/hitl/<test>/@: GDB memory dumps of the
management-unit results structs for C/A′, register dumps of the processing
element for A/B/B_sf.
-}
module Bittide.Instances.Hitl.DecodeDemo.Driver where

import Clash.Prelude

import Bittide.ClockControl.Config (defCcConf)
import Bittide.Hitl
import Bittide.Instances.Domains (GthTx)
import Bittide.Instances.Hitl.Setup (FpgaCount, LinkCount, demoRigInfo, fpgaSetup)
import Bittide.Instances.Hitl.Utils.Driver
import Bittide.Instances.Hitl.Utils.Gdb (initGdb)
import Bittide.Instances.Hitl.Utils.MemoryMap (getPathAddress)
import Bittide.Instances.Hitl.Utils.OpenOcd (parseBootTapInfo, parseTapInfo)
import Bittide.Instances.Hitl.Utils.Serial (initSerial)
import Bittide.Instances.Hitl.Utils.Usb (resetUsbDeviceByLocation)
import Bittide.Instances.Hitl.Utils.Utils (dumpCcSamples)
import Bittide.Wishbone (TimeCmd (Capture))
import Clash.Class.BitPackC (BitPackC)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently, mapConcurrently_)
import Control.Concurrent.Async.Extra (zipWithConcurrently, zipWithConcurrently3_)
import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.IO.Class
import Data.Bifunctor (Bifunctor (bimap))
import Data.Maybe (fromJust, mapMaybe)
import Data.Typeable (Typeable)
import Data.Vector.Internal.Check (HasCallStack)
import Gdb (Gdb)
import Numeric (readHex)
import Project.Chan (readUntilLine, waitForLine)
import Project.FilePath
import Project.Handle (assertEither, expectRight)
import Protocols.MemoryMap (MemoryMap)
import System.Exit
import System.FilePath
import Vivado.Tcl (HwTarget)
import Vivado.VivadoM (VivadoM)
import "bittide-extra" Control.Exception.Extra (brackets)

import qualified Bittide.Calculator as Calc
import qualified Bittide.Instances.Hitl.DecodeDemo.MemoryMaps as MemoryMaps
import qualified Bittide.Instances.Hitl.Utils.OpenOcd as Ocd
import qualified Clash.Sized.Vector as V
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Gdb
import qualified System.Timeout.Extra as T

-- Workload shape. The hardware variants run the full token count; the
-- (much slower) software variants run fewer tokens to keep wall clock down —
-- still hundreds of latency samples each.
vectorWordsC :: Unsigned 32
vectorWordsC = 64

layersPerTokenC :: Unsigned 32
layersPerTokenC = 16

hwTokenCount, cTokenCount, aPrimeTokenCount :: Unsigned 32
hwTokenCount = 10_000
-- A variant-C token costs ~0.23 s on the rig (16 layers x 16 serial
-- hop-services on the management units), so counts and timeouts are sized
-- for that.
cTokenCount = 500
aPrimeTokenCount = 200

{- | The delay in clock cycles between two PEs which is not accounted for by
the `captureUgn` component (see the wire demo driver for the full story).
The decode PE has the same taps and the same one-register output stage as the
wire demo PE, so the constant is expected to carry over; it is re-derived
empirically on the rig if variant A shows checksum failures
(`first_fail_cycle` localizes the offset).
-}
internalDelay :: Int
internalDelay = -4

data DecodeNodeConfig = DecodeNodeConfig
  { firstCycle :: Unsigned 64
  -- ^ Variant A: this node's first window. Variant B/C/A': start gate.
  , readLink :: Index LinkCount
  , writeLink :: Index LinkCount
  , isInjector :: Bool
  , localPattern :: BitVector 64
  , expectedWindowA :: BitVector 64
  , expectedWindowB :: BitVector 64
  }
  deriving (Show)

data DecodeSchedule = DecodeSchedule
  { nodes :: Vec FpgaCount DecodeNodeConfig
  , lapOffset :: Unsigned 64
  -- ^ Full-ring traversal time (identical in every node's syntonized clock)
  }
  deriving (Show)

{- | Build the ring schedule from the hardware UGNs, wire-demo style: node
@k+1@'s first window is where node @k@'s first transmitted word (fire + one
register stage) arrives, mapped through the counter map. The ring closes:
chaining once around yields the lap offset.
-}
generateDecodeSchedule ::
  Vec FpgaCount (Calc.FpgaId, Vec LinkCount (Index FpgaCount)) ->
  Vec FpgaCount (Vec LinkCount (Unsigned 64, Unsigned 64)) ->
  -- | Vector words (the checksums depend on it)
  Unsigned 32 ->
  Unsigned 64 ->
  DecodeSchedule
generateDecodeSchedule fpgaTable ugnParts vectorWords startCycle =
  DecodeSchedule
    { nodes = genConfig <$> indicesI <*> firstCycles
    , lapOffset = closedCycle - startCycle
    }
 where
  mapCycle :: Unsigned 64 -> Index FpgaCount -> Index FpgaCount -> Unsigned 64
  mapCycle srcCycle src dst = if dstCycleI < 0 then maxBound else fromInteger dstCycleI
   where
    dstCycleI = toInteger srcCycle + counterMap !! src Map.! dst
    ugnPartsI = map (map (bimap toInteger toInteger)) ugnParts
    counterMap = Calc.toCounterMap internalDelay (Calc.toFpgaIndexed fpgaTable ugnPartsI)

  -- First-window cycles for nodes 0..7 plus the closing hop back to node 0.
  hop :: (Index FpgaCount, Unsigned 64) -> (Index FpgaCount, Unsigned 64)
  hop (k, cycle_) = (nextNode k, mapCycle (cycle_ + 1) k (nextNode k))

  chain :: [(Index FpgaCount, Unsigned 64)]
  chain = L.take (natToNum @FpgaCount + 1) (L.iterate hop (0, startCycle))

  firstCycles :: Vec FpgaCount (Unsigned 64)
  firstCycles = fromJust (V.fromList (L.map snd (L.init chain)))

  closedCycle = snd (L.last chain)

  nextNode, prevNode :: Index FpgaCount -> Index FpgaCount
  nextNode k = satSucc SatWrap k
  prevNode k = satPred SatWrap k

  toLinkIndex :: Index FpgaCount -> Index FpgaCount -> Index LinkCount
  toLinkIndex currentFpga targetFpga =
    fromJust $ elemIndex targetFpga (snd (fpgaTable !! currentFpga))

  patterns :: Vec FpgaCount (BitVector 64)
  patterns = fromJust (V.fromList (L.map (resize . (.dna)) demoRigInfo))

  vw, iSum :: BitVector 64
  vw = fromIntegral vectorWords
  iSum = fromIntegral (vectorWords * (vectorWords - 1) `div` 2)

  grandTotal = vw * sum patterns + fromIntegral (natToNum @FpgaCount :: Integer) * iSum

  genConfig :: Index FpgaCount -> Unsigned 64 -> DecodeNodeConfig
  genConfig k firstCycle =
    DecodeNodeConfig
      { firstCycle
      , readLink = toLinkIndex k (prevNode k)
      , writeLink = toLinkIndex k (nextNode k)
      , isInjector = k == 0
      , localPattern = patterns !! k
      , expectedWindowA = vw * prefix + fromIntegral (toInteger k) * iSum
      , expectedWindowB = grandTotal
      }
   where
    prefix = L.sum (L.take (fromIntegral k) (toList patterns))

-- | Read the current time in clock cycles (requires a halted CPU).
readCurrentTime :: (HasCallStack) => MemoryMap -> Gdb -> IO (Unsigned 64)
readCurrentTime mm gdb = do
  commandAddress <- expectRight $ getPathAddress mm ["0", "Timer", "command"]
  scratchAddress <- expectRight $ getPathAddress mm ["0", "Timer", "scratchpad"]
  Gdb.writeLe gdb commandAddress Capture
  Gdb.readLe gdb scratchAddress

-- | Read the hardware UGNs (requires a halted CPU); wire-demo logic.
readHardwareUgns :: MemoryMap -> (HwTarget, DeviceInfo) -> Gdb -> IO [(Unsigned 64, Unsigned 64)]
readHardwareUgns mm (_, d) gdb = do
  let
    getUgnRegister reg = expectRight $ getPathAddress mm ["0", "CaptureUgns", reg]
    adjustLocalCounter :: (Unsigned 64, Unsigned 64, Signed 32) -> (Unsigned 64, Unsigned 64)
    adjustLocalCounter (localCounter, remoteCounter, delta) =
      (checkedFromIntegral (toInteger localCounter + toInteger delta), remoteCounter)
  liftIO $ putStrLn $ "Getting UGNs for device " <> d.deviceId
  localCounters <- Gdb.readLe @(Vec LinkCount (Unsigned 64)) gdb =<< getUgnRegister "local_counter"
  remoteCounters <- Gdb.readLe @(Vec LinkCount (Unsigned 64)) gdb =<< getUgnRegister "remote_counter"
  deltas <- Gdb.readLe @(Vec LinkCount (Signed 32)) gdb =<< getUgnRegister "elastic_buffer_delta"
  pure $ adjustLocalCounter <$> toList (zip3 localCounters remoteCounters deltas)

{- | Parse @[MU] RESULTS @ <hex> CONFIG @ <hex>@ from the lines preceding the
UGN sentinel. Both statics live at the same address on every node (identical
ELFs), but we parse per node anyway.
-}
parseStructAddresses :: (HasCallStack) => [String] -> IO (Integer, Integer)
parseStructAddresses ls = case mapMaybe parse ls of
  (a : _) -> pure a
  [] -> fail $ "No 'RESULTS @ .. CONFIG @ ..' line in:\n" <> unlines ls
 where
  parse l
    | ["[MU]", "RESULTS", "@", r, "CONFIG", "@", c] <- words l
    , [(rAddr, "")] <- readHex r
    , [(cAddr, "")] <- readHex c =
        Just (rAddr, cAddr)
    | otherwise = Nothing

-- Field offsets of the firmware's #[repr(C)] DecodeConfig
-- (firmware-support/bittide-sys/src/decode_demo.rs). Keep in sync!
data McVariant = VariantC | VariantAPrime deriving (Eq, Show)

mcVariantCode :: McVariant -> Unsigned 32
mcVariantCode VariantC = 2
mcVariantCode VariantAPrime = 3

{- | Write the management-unit engine configuration struct field by field
(the CPU is halted, so ordering is irrelevant; @go@ starts the engine on
resume).
-}
writeDecodeConfig ::
  (HasCallStack) =>
  Gdb ->
  Integer ->
  McVariant ->
  Unsigned 32 ->
  -- | (lap offset, layer period, token period) for A' schedules
  (Unsigned 32, Unsigned 32, Unsigned 32) ->
  DecodeNodeConfig ->
  IO ()
writeDecodeConfig gdb base variant tokenCount (lapOff, layerPer, tokenPer) node = do
  let fieldAt off = base + off
  Gdb.writeLe @(Unsigned 32) gdb (fieldAt 8) (if node.isInjector then 1 else 0)
  Gdb.writeLe @(Unsigned 32) gdb (fieldAt 12) (fromIntegral node.readLink)
  Gdb.writeLe @(Unsigned 32) gdb (fieldAt 16) (fromIntegral node.writeLink)
  Gdb.writeLe @(Unsigned 32) gdb (fieldAt 20) layersPerTokenC
  Gdb.writeLe @(Unsigned 32) gdb (fieldAt 24) tokenCount
  Gdb.writeLe @(Unsigned 32) gdb (fieldAt 28) vectorWordsC
  Gdb.writeLe @(Unsigned 32) gdb (fieldAt 32) 0 -- compute_cycles
  -- Far above one token's duration: a poll that times out abandons a frame
  -- the ring then permanently misses, so this is a last resort, not a pacing
  -- mechanism.
  Gdb.writeLe @(Unsigned 32) gdb (fieldAt 36) 400_000_000 -- poll_timeout: 2 s
  Gdb.writeLe @(Unsigned 64) gdb (fieldAt 40) node.firstCycle
  Gdb.writeLe @(Unsigned 32) gdb (fieldAt 48) lapOff
  Gdb.writeLe @(Unsigned 32) gdb (fieldAt 52) layerPer
  Gdb.writeLe @(Unsigned 32) gdb (fieldAt 56) tokenPer
  Gdb.writeLe @(Unsigned 32) gdb (fieldAt 60) 0 -- hist_base
  Gdb.writeLe @(Unsigned 32) gdb (fieldAt 64) 4 -- hist_shift: 16-cycle bins
  Gdb.writeLe @(BitVector 64) gdb (fieldAt 72) node.localPattern
  Gdb.writeLe @(BitVector 64) gdb (fieldAt 80) node.expectedWindowA
  Gdb.writeLe @(BitVector 64) gdb (fieldAt 88) node.expectedWindowB
  Gdb.writeLe @(Unsigned 32) gdb (fieldAt 4) (mcVariantCode variant) -- go, starts on resume

-- Size of the firmware's #[repr(C)] DecodeResults. Keep in sync!
decodeResultsSize :: Integer
decodeResultsSize = 3120

-- | Results summary read straight from the management unit's memory.
data McResults = McResults
  { tokensDone :: Unsigned 32
  , checksumFails :: Unsigned 32
  , lostFrames :: Unsigned 32
  , deadlinesMissed :: Unsigned 32
  , minLatency :: Unsigned 32
  , maxLatency :: Unsigned 32
  }
  deriving (Show)

readMcResults :: (HasCallStack) => Gdb -> Integer -> IO McResults
readMcResults gdb base = do
  tokensDone <- Gdb.readLe gdb (base + 8)
  checksumFails <- Gdb.readLe gdb (base + 12)
  lostFrames <- Gdb.readLe gdb (base + 16)
  deadlinesMissed <- Gdb.readLe gdb (base + 20)
  minLatency <- Gdb.readLe gdb (base + 32)
  maxLatency <- Gdb.readLe gdb (base + 36)
  pure McResults{tokensDone, checksumFails, lostFrames, deadlinesMissed, minLatency, maxLatency}

-- | Get the address of a decode-PE (or credit-link) register.
peRegister :: (HasCallStack, Num a) => String -> String -> IO a
peRegister dev reg =
  expectRight $ getPathAddress MemoryMaps.managementUnit ["0", dev, reg]

data PeVariant = VariantA | VariantB | VariantBsf deriving (Eq, Show)

peVariantName :: PeVariant -> String
peVariantName VariantA = "a"
peVariantName VariantB = "b"
peVariantName VariantBsf = "bsf"

writePeConfig ::
  (HasCallStack) =>
  Gdb ->
  PeVariant ->
  -- | (vector words, token count)
  (Unsigned 32, Unsigned 32) ->
  -- | (lap offset, layer period, token period)
  (Unsigned 32, Unsigned 32, Unsigned 32) ->
  -- | Histogram base
  Unsigned 32 ->
  DecodeNodeConfig ->
  IO ()
writePeConfig gdb variant (vectorWords, tokenCount) (lapOff, layerPer, tokenPer) histBase node = do
  let
    w :: forall a. (BitPackC a, Typeable a, NFDataX a) => String -> a -> IO ()
    w reg value = do
      addr <- peRegister "DecodePeConfig" reg
      Gdb.writeLe @a gdb addr value
  w "read_link" (Just node.readLink)
  w "write_link" (Just node.writeLink)
  w "is_injector" node.isInjector
  w "mode" (if variant == VariantA then 0 else 1 :: Unsigned 8)
  w "first_cycle" node.firstCycle
  w "lap_offset" lapOff
  w "layer_period" layerPer
  w "layers_per_token" (fromIntegral layersPerTokenC :: Unsigned 16)
  w "token_period" tokenPer
  w "token_count" tokenCount
  w "vector_words" (fromIntegral vectorWords :: Unsigned 16)
  w "compute_cycles" (0 :: Unsigned 32)
  w "local_pattern" node.localPattern
  w "expected_window_a" node.expectedWindowA
  w "expected_window_b" node.expectedWindowB
  w "hist_base" histBase

writeCreditLinkKnobs :: (HasCallStack) => Gdb -> PeVariant -> IO ()
writeCreditLinkKnobs gdb variant = do
  cutThroughAddr <- peRegister "CreditLinkConfig" "cut_through"
  Gdb.writeLe gdb cutThroughAddr (variant /= VariantBsf)

armDecodePe :: (HasCallStack) => Gdb -> IO ()
armDecodePe gdb = do
  armAddr <- peRegister "DecodePeConfig" "arm"
  Gdb.writeLe gdb armAddr True

data PeStatus = PeStatus
  { tokensDone :: Unsigned 32
  , checksumFailCount :: Unsigned 32
  , firstFailCycle :: Unsigned 64
  , minLatency :: Unsigned 32
  , maxLatency :: Unsigned 32
  , lastLatency :: Unsigned 32
  , done :: Bool
  , hist :: Vec 16 (Unsigned 32)
  }
  deriving (Show)

readPeStatus :: (HasCallStack) => Gdb -> IO PeStatus
readPeStatus gdb = do
  let r :: forall a. (BitPackC a, Typeable a, NFDataX a) => String -> IO a
      r reg = Gdb.readLe @a gdb =<< peRegister "DecodePeConfig" reg
  tokensDone <- r "tokens_done"
  checksumFailCount <- r "checksum_fail_count"
  firstFailCycle <- r "first_fail_cycle"
  minLatency <- r "min_latency"
  maxLatency <- r "max_latency"
  lastLatency <- r "last_latency"
  done <- r "done"
  hist <- r "hist"
  pure
    PeStatus
      { tokensDone
      , checksumFailCount
      , firstFailCycle
      , minLatency
      , maxLatency
      , lastLatency
      , done
      , hist
      }

data ClStatus = ClStatus
  { creditsConsumed :: Unsigned 32
  , creditsReturned :: Unsigned 32
  , creditsGranted :: Unsigned 32
  , framesReceived :: Unsigned 32
  , headerErrors :: Unsigned 32
  , creditErrors :: Unsigned 32
  , noCreditDrops :: Unsigned 32
  , timeoutCount :: Unsigned 32
  , minCreditRtt :: Unsigned 32
  , maxCreditRtt :: Unsigned 32
  }
  deriving (Show)

readClStatus :: (HasCallStack) => Gdb -> IO ClStatus
readClStatus gdb = do
  let r reg = Gdb.readLe @(Unsigned 32) gdb =<< peRegister "CreditLinkConfig" reg
  creditsConsumed <- r "credits_consumed"
  creditsReturned <- r "credits_returned"
  creditsGranted <- r "credits_granted"
  framesReceived <- r "frames_received"
  headerErrors <- r "header_errors"
  creditErrors <- r "credit_errors"
  noCreditDrops <- r "no_credit_drops"
  timeoutCount <- r "timeout_count"
  minCreditRtt <- r "min_credit_rtt"
  maxCreditRtt <- r "max_credit_rtt"
  pure
    ClStatus
      { creditsConsumed
      , creditsReturned
      , creditsGranted
      , framesReceived
      , headerErrors
      , creditErrors
      , noCreditDrops
      , timeoutCount
      , minCreditRtt
      , maxCreditRtt
      }

-- | Poll a node's @done@ register until set (or time out).
waitPeDone :: (HasCallStack) => String -> Gdb -> IO ()
waitPeDone who gdb = do
  doneAddr <- peRegister "DecodePeConfig" "done"
  let poll attempt = do
        done <- Gdb.readLe @Bool gdb doneAddr
        unless done
          $ if (attempt :: Int) > 600
            then fail $ who <> ": decode PE did not finish within 60 s"
            else threadDelay 100_000 >> poll (attempt + 1)
  poll 0

driver ::
  (HasCallStack) =>
  String ->
  [(HwTarget, DeviceInfo)] ->
  VivadoM ExitCode
driver testName targets = do
  liftIO
    . putStrLn
    $ "Running decode demo driver for targets "
    <> show ((\(_, info) -> info.deviceId) <$> targets)

  projectDir <- liftIO $ findParentContaining "cabal.project"
  let hitlDir = projectDir </> "_build/hitl" </> testName

  forM_ targets (assertProbe "probe_test_start")
  liftIO $ forM_ targets $ \(_, d) -> resetUsbDeviceByLocation d.usbAdapterLocation

  let
    -- BOOT / MU / CC IDs
    expectedJtagIds = [0x0514C001, 0x1514C001, 0x2514C001]
    toInitArgs (_, deviceInfo) targetIndex =
      Ocd.InitOpenOcdArgs{deviceInfo, expectedJtagIds, hitlDir, targetIndex}
    initArgs = L.zipWith toInitArgs targets [0 ..]
    optionalBootInitArgs = L.repeat def{Ocd.logPrefix = "boot-", Ocd.initTcl = "vexriscv_boot_init.tcl"}
    openOcdBootStarts = liftIO <$> L.zipWith Ocd.initOpenOcd initArgs optionalBootInitArgs

  let serialStarts = liftIO <$> L.zipWith (initSerial hitlDir) targets [0 ..]
  brackets serialStarts (liftIO . snd) $ \(L.map fst -> serials) -> do
    brackets openOcdBootStarts (liftIO . (.cleanup)) $ \initOcdsData -> do
      let bootTapInfos = parseBootTapInfo <$> initOcdsData
      Gdb.withGdbs (L.length targets) $ \bootGdbs -> do
        liftIO
          $ zipWithConcurrently3_ (initGdb hitlDir "decode-demo-boot") bootGdbs bootTapInfos targets
        liftIO $ mapConcurrently_ ((assertEither =<<) . Gdb.loadBinary) bootGdbs
        liftIO $ mapConcurrently_ Gdb.continue bootGdbs
        liftIO
          $ T.tryWithTimeout T.PrintActionTime "Waiting for boot" 60_000_000
          $ forM_ serials
          $ \serial -> waitForLine serial "[BT] Going into infinite loop.."

  let
    optionalInitArgs = L.repeat def
    openOcdStarts = liftIO <$> L.zipWith Ocd.initOpenOcd initArgs optionalInitArgs

  brackets openOcdStarts (liftIO . (.cleanup)) $ \initOcdsData -> do
    let
      allTapInfos = parseTapInfo expectedJtagIds <$> initOcdsData
      _bootTapInfos, managementUnitTapInfos, clockControlTapInfos :: [Ocd.TapInfo]
      (_bootTapInfos, managementUnitTapInfos, clockControlTapInfos)
        | all (== L.length expectedJtagIds) (L.length <$> allTapInfos)
        , [boots, mus, ccs] <- L.transpose allTapInfos =
            (boots, mus, ccs)
        | otherwise = error "Unexpected number of OpenOCD taps initialized"

    Gdb.withGdbs (L.length targets) $ \clockControlGdbs -> do
      liftIO
        $ zipWithConcurrently3_
          (initGdb hitlDir "decode-demo-clock-control")
          clockControlGdbs
          clockControlTapInfos
          targets
      liftIO $ mapConcurrently_ ((assertEither =<<) . Gdb.loadBinary) clockControlGdbs

      Gdb.withGdbs (L.length targets) $ \managementUnitGdbs -> do
        liftIO
          $ zipWithConcurrently3_
            (initGdb hitlDir "decode-demo-management-unit")
            managementUnitGdbs
            managementUnitTapInfos
            targets
        liftIO $ mapConcurrently_ ((assertEither =<<) . Gdb.loadBinary) managementUnitGdbs

        brackets serialStarts (liftIO . snd) $ \(L.map fst -> serials) -> do
          let goDumpCcSamples = dumpCcSamples MemoryMaps.clockControl hitlDir (defCcConf (natToNum @FpgaCount)) clockControlGdbs
          liftIO $ mapConcurrently_ Gdb.continue clockControlGdbs
          liftIO $ mapConcurrently_ Gdb.continue managementUnitGdbs

          -- Wait for the UGN sentinel, collecting the struct-address lines
          -- printed just before it.
          preSentinelLines <-
            liftIO
              $ T.tryWithTimeoutOn
                T.PrintActionTime
                "Waiting for captured UGNs"
                60_000_000
                goDumpCcSamples
              $ forConcurrently serials
              $ \serial -> readUntilLine serial "[MU] Printed all hardware UGNs"
          structAddrs <- liftIO $ mapM parseStructAddresses preSentinelLines

          liftIO $ putStrLn "Getting UGNs for all targets"
          liftIO $ mapConcurrently_ Gdb.interrupt managementUnitGdbs
          ugnPairsTable <-
            liftIO
              $ zipWithConcurrently (readHardwareUgns MemoryMaps.managementUnit) targets managementUnitGdbs
          let ugnPairsTableV = fromJust . V.fromList $ fromJust . V.fromList <$> ugnPairsTable
          liftIO $ Calc.printAllIgns ugnPairsTableV fpgaSetup

          currentTime0 <-
            liftIO $ readCurrentTime MemoryMaps.managementUnit (L.head managementUnitGdbs)
          let
            secondsFromNow :: Unsigned 64 -> Unsigned 64
            secondsFromNow s = currentTime0 + s * natToNum @(PeriodToCycles GthTx (Seconds 1))
            -- The schedule is regenerated per variant with a fresh start
            -- cycle; the ring structure (links, patterns, lap offset) is
            -- identical every time.
            scheduleAt vw start = generateDecodeSchedule fpgaSetup ugnPairsTableV vw start
            probeSchedule = scheduleAt vectorWordsC (secondsFromNow 2)
            lapOff = probeSchedule.lapOffset
          liftIO $ putStrLn $ "Ring lap offset (cycles): " <> show lapOff

          let
            runMcVariant variant tokenCount fileTag = do
              putStrLn $ "Running variant " <> show variant
              currentTime <- readCurrentTime MemoryMaps.managementUnit (L.head managementUnitGdbs)
              let
                -- Generous start gate: every node must finish ring-buffer
                -- alignment before anyone transmits.
                sched = scheduleAt vectorWordsC (currentTime + 2 * natToNum @(PeriodToCycles GthTx (Seconds 1)))
                -- A' management-unit calendar: unlike the hardware schedule
                -- (35 cycles per hop), each software hop costs the CPU its
                -- per-frame service time (~9k cycles), so windows stagger by
                -- a software hop stride and a lap is a full software ring
                -- traversal.
                aPrimeHop = 25_000 :: Unsigned 32
                aPrimeLapOff = natToNum @FpgaCount * aPrimeHop
                aPrimeLayerPer = 2 * aPrimeLapOff + 100_000
                aPrimeTokenPer = fromIntegral layersPerTokenC * aPrimeLayerPer + 100_000
                -- Node k's windows shift by k software hops.
                aPrimeNode k node
                  | variant == VariantAPrime =
                      node{firstCycle = node.firstCycle + fromIntegral k * widenU aPrimeHop}
                  | otherwise = node
                widenU :: Unsigned 32 -> Unsigned 64
                widenU = resize
              forM_
                (L.zip3 managementUnitGdbs structAddrs (L.zipWith aPrimeNode [0 :: Int ..] (toList sched.nodes)))
                $ \(gdb, (_, cfgAddr), node) ->
                  writeDecodeConfig
                    gdb
                    cfgAddr
                    variant
                    tokenCount
                    (aPrimeLapOff, aPrimeLayerPer, aPrimeTokenPer)
                    node
              mapConcurrently_ Gdb.continue managementUnitGdbs
              let
                doneLine = case variant of
                  VariantC -> "[MU] Variant C done"
                  VariantAPrime -> "[MU] Variant A' done"
                -- On timeout: freeze the management units and print every
                -- node's result counters, so a stall pinpoints itself.
                onTimeout = do
                  goDumpCcSamples
                  mapConcurrently_ Gdb.interrupt managementUnitGdbs
                  forM_ (L.zip3 [0 :: Int ..] managementUnitGdbs structAddrs)
                    $ \(n, gdb, (resAddr, _)) -> do
                      result <- try @SomeException (readMcResults gdb resAddr)
                      putStrLn $ "  TIMEOUT node " <> show n <> ": " <> show result
              T.tryWithTimeoutOn T.PrintActionTime ("Waiting for " <> show variant) 480_000_000 onTimeout
                $ forM_ serials
                $ \serial -> waitForLine serial doneLine
              mapConcurrently_ Gdb.interrupt managementUnitGdbs
              results <- forM (L.zip3 [0 :: Int ..] managementUnitGdbs structAddrs)
                $ \(n, gdb, (resAddr, _)) -> do
                  Gdb.dumpMemoryRegion
                    gdb
                    (hitlDir </> "results-" <> fileTag <> "-" <> show n <.> "bin")
                    resAddr
                    (resAddr + decodeResultsSize)
                  readMcResults gdb resAddr
              forM_ (L.zip [0 :: Int ..] results) $ \(n, r) ->
                putStrLn $ "  node " <> show n <> ": " <> show r
              pure results

            checkMcResults variant tokenCount results = do
              let lostBudget = tokenCount `div` 100 -- <1% lost tokens
              forM_ (L.zip [0 :: Int ..] results) $ \(n, r) -> do
                when (r.checksumFails /= 0)
                  $ fail (show variant <> " node " <> show n <> " checksum failures: " <> show r.checksumFails)
                when (r.tokensDone + r.lostFrames + r.deadlinesMissed < tokenCount)
                  $ fail (show variant <> " node " <> show n <> " incomplete: " <> show r)
                when (r.lostFrames + r.deadlinesMissed > lostBudget)
                  $ fail (show variant <> " node " <> show n <> " too many lost/missed: " <> show r)

          -- Variant C, then A' — strictly before the one-shot mux arming.
          cResults <- liftIO $ runMcVariant VariantC cTokenCount "c"
          liftIO $ checkMcResults VariantC cTokenCount cResults
          aPrimeResults <- liftIO $ runMcVariant VariantAPrime aPrimeTokenCount "aprime"
          liftIO $ checkMcResults VariantAPrime aPrimeTokenCount aPrimeResults

          -- Arm the programmable mux: the links belong to the processing
          -- element from here on. The magic-word framing of the credit link
          -- requires the local counter to stay below 2^47.
          liftIO $ do
            currentTime <- readCurrentTime MemoryMaps.managementUnit (L.head managementUnitGdbs)
            when (currentTime >= bit 47) $ fail "Local counter too large for credit-link framing"
            let muxCycle = currentTime + 2 * natToNum @(PeriodToCycles GthTx (Seconds 1))
            forM_ managementUnitGdbs $ \gdb -> do
              firstBAddr <- peRegister "ProgrammableMux" "first_b_cycle"
              armAddr <- peRegister "ProgrammableMux" "arm"
              Gdb.writeLe gdb firstBAddr muxCycle
              Gdb.writeLe gdb armAddr True
            threadDelay 3_000_000

          let
            runPeVariant variant vw tokenCount fileTag = do
              putStrLn $ "Running variant " <> show variant <> " (" <> fileTag <> ")"
              currentTime <- readCurrentTime MemoryMaps.managementUnit (L.head managementUnitGdbs)
              let
                layerPer = truncateB (2 * lapOff) + fromIntegral vw + 256 :: Unsigned 32
                tokenPer = fromIntegral layersPerTokenC * layerPer + 1024
                predictedTokenLatency =
                  fromIntegral (layersPerTokenC - 1)
                    * layerPer
                    + truncateB (2 * lapOff)
                    + fromIntegral vw
                sched = scheduleAt vw (currentTime + 2 * natToNum @(PeriodToCycles GthTx (Seconds 1)))
                histBase n
                  | variant == VariantA || n == (0 :: Int) =
                      satSub SatZero predictedTokenLatency 8
                  | otherwise = 100 -- relays histogram credit RTTs in B modes
              forM_ managementUnitGdbs $ \gdb -> writeCreditLinkKnobs gdb variant
              forM_ (L.zip3 [0 ..] managementUnitGdbs (toList sched.nodes))
                $ \(n, gdb, node) ->
                  writePeConfig gdb variant (vw, tokenCount) (truncateB lapOff, layerPer, tokenPer) (histBase n) node
              -- Arm relays first, the injector (node 0) last.
              forM_ (L.reverse managementUnitGdbs) armDecodePe
              forM_ (L.zip [0 :: Int ..] managementUnitGdbs) $ \(n, gdb) ->
                waitPeDone (show variant <> " node " <> show n) gdb
              statuses <- mapM readPeStatus managementUnitGdbs
              clStatuses <- mapM readClStatus managementUnitGdbs
              forM_ (L.zip3 [0 :: Int ..] statuses clStatuses) $ \(n, st, cl) -> do
                putStrLn $ "  node " <> show n <> ": " <> show st{hist = repeat 0}
                when (variant /= VariantA)
                  $ putStrLn
                  $ "  node "
                  <> show n
                  <> " credit link: "
                  <> show cl
                writeFile
                  (hitlDir </> "pe-" <> fileTag <> "-" <> show n <.> "txt")
                  ( unlines
                      $ [ "tokens_done " <> show st.tokensDone
                        , "checksum_fail_count " <> show st.checksumFailCount
                        , "first_fail_cycle " <> show st.firstFailCycle
                        , "min_latency " <> show st.minLatency
                        , "max_latency " <> show st.maxLatency
                        , "hist_base " <> show (histBase n)
                        , "credits_consumed " <> show cl.creditsConsumed
                        , "credits_returned " <> show cl.creditsReturned
                        , "credits_granted " <> show cl.creditsGranted
                        , "frames_received " <> show cl.framesReceived
                        , "header_errors " <> show cl.headerErrors
                        , "credit_errors " <> show cl.creditErrors
                        , "no_credit_drops " <> show cl.noCreditDrops
                        , "timeout_count " <> show cl.timeoutCount
                        , "min_credit_rtt " <> show cl.minCreditRtt
                        , "max_credit_rtt " <> show cl.maxCreditRtt
                        ]
                      <> ["hist " <> L.unwords (L.map show (toList st.hist))]
                  )
              pure (statuses, clStatuses)

            checkPeVariant variant tokenCount statuses clStatuses = do
              let expectedTransfers =
                    2 * fromIntegral layersPerTokenC * tokenCount :: Unsigned 32
              forM_ (L.zip3 [0 :: Int ..] statuses clStatuses) $ \(n, st, cl) -> do
                let who = show variant <> " node " <> show n
                when (st.checksumFailCount /= 0)
                  $ fail (who <> ": checksum failures, first at " <> show st.firstFailCycle)
                when (st.tokensDone /= tokenCount)
                  $ fail (who <> ": incomplete: " <> show st.tokensDone)
                when (st.maxLatency - st.minLatency > 4)
                  $ fail
                    ( who
                        <> ": latency spread "
                        <> show st.minLatency
                        <> ".."
                        <> show st.maxLatency
                    )
                when (variant /= VariantA) $ do
                  when (cl.headerErrors /= 0 || cl.creditErrors /= 0 || cl.noCreditDrops /= 0 || cl.timeoutCount /= 0)
                    $ fail (who <> ": credit link errors: " <> show cl)
                  when
                    ( L.any
                        (/= fromIntegral expectedTransfers)
                        [cl.creditsConsumed, cl.creditsReturned, cl.creditsGranted, cl.framesReceived]
                    )
                    $ fail (who <> ": credit accounting mismatch: " <> show cl)

          -- Probe stage: variant A with single-word vectors and few tokens.
          -- Degenerates to (nearly) the wire demo's shape, so it validates
          -- the schedule and the internalDelay constant before the full-width
          -- runs; its first_fail_cycle localizes any tap-timing surprise.
          (probeStatuses, probeCl) <- liftIO $ runPeVariant VariantA 1 100 "probe"
          liftIO $ checkPeVariant VariantA 100 probeStatuses probeCl

          (aStatuses, aCl) <- liftIO $ runPeVariant VariantA vectorWordsC hwTokenCount "a"
          liftIO $ checkPeVariant VariantA hwTokenCount aStatuses aCl
          (bStatuses, bCl) <- liftIO $ runPeVariant VariantB vectorWordsC hwTokenCount "b"
          liftIO $ checkPeVariant VariantB hwTokenCount bStatuses bCl
          (bsfStatuses, bsfCl) <- liftIO $ runPeVariant VariantBsf vectorWordsC hwTokenCount "bsf"
          liftIO $ checkPeVariant VariantBsf hwTokenCount bsfStatuses bsfCl

          liftIO goDumpCcSamples

          liftIO $ do
            let
              aLat = (L.head aStatuses).maxLatency
              bLat = (L.head bStatuses).maxLatency
              bsfLat = (L.head bsfStatuses).maxLatency
            putStrLn $ "Token latency A:    " <> show aLat
            putStrLn $ "Token latency B:    " <> show bLat
            putStrLn $ "Token latency B_sf: " <> show bsfLat
            unless (bsfLat > bLat)
              $ fail "Store-and-forward should be slower than cut-through"

          pure ExitSuccess
