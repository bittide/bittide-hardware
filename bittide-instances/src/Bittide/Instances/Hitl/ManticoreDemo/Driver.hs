-- SPDX-FileCopyrightText: 2026 QBayLogic
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

{- | Driver for the Manticore demo HITL test.

Runs a Manticore program on one FPGA of the rig and reads its @$display@ trace
back, mirroring the (board-verified) KCU105 @run_manifest.py@ host flow, but
over the management-unit (MU) GDB instead of JTAG-to-AXI, and through a
memory-mapped gmem region on the MU bus instead of a flat AXI memory:

  1. Bring up the boot CPU (Si539x clock config) — reuse the demo boot
     firmware; it is independent of the user core.
  2. Attach GDB to the (halted) MU; its bus reaches the chip's Wishbone host
     registers (ManticoreControl) and the gmem region (ManticoreGmem), so the
     driver pokes them directly — no Manticore-specific MU firmware needed.
  3. Load the program image (manifest.json + exec.bin streams produced by
     .github/scripts/manticore_compile_program.sh) into the gmem region with a
     bulk GDB @restore@ (one round-trip per binary, not per word).
  4. Run each initializer (CMD_START, expect FINISH), then the main program;
     on each FLUSH (a @$display@) read the trace record from the gmem region
     and resume, until FINISH.
  5. Check the collected trace against the golden values.

NB milestone 1: single chip, MU halted + poked over GDB. The MU/clock-control
firmware is not run (a single chip needs no link startup / clock grooming).
-}
module Bittide.Instances.Hitl.ManticoreDemo.Driver where

import Prelude

import Bittide.ClockControl.Config (defCcConf)
import Bittide.Hitl (DeviceInfo (..))
import Bittide.Instances.Domains (GthTx)
import Bittide.Instances.Hitl.ManticoreDemo.Latencies (dirName, icAt, seamConfig)
import Bittide.Instances.Hitl.Setup (FpgaCount, LinkCount, fpgaSetup)
import Bittide.Instances.Hitl.Utils.Driver (assertProbe)
import Bittide.Instances.Hitl.Utils.Gdb (initGdb)
import Bittide.Instances.Hitl.Utils.MemoryMap (getPathAddress)
import Bittide.Instances.Hitl.Utils.Picocom (initPicocom)
import Bittide.Instances.Hitl.Utils.Relabel (
  RelabelPlan (..),
  computeRelabel,
  hardwareUgnEdges,
  readCurrentTime,
  writeCorrections,
  writeReleaseCycle,
 )
import Bittide.Instances.Hitl.Utils.Usb (resetUsbDeviceByLocation)
import Bittide.Instances.Hitl.Utils.Utils (dumpCcSamples)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently_, mapConcurrently_)
import Control.Concurrent.Async.Extra (zipWithConcurrently, zipWithConcurrently3_)
import Control.Exception (SomeException, finally, throwIO, try)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.Aeson (Value (Array, Number, Object, String))
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Default (def)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.String.Interpolate (i, __i)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric (showHex)
import Project.Chan (waitForLine)
import Project.FilePath (findParentContaining)
import Project.Handle (assertEither)
import System.Directory (doesFileExist, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Vivado.Tcl (
  HwTarget,
  current_hw_ila,
  execCmd_,
  get_hw_ilas,
  openHwTarget,
  refresh_hw_device,
  run_hw_ila,
 )
import Vivado.VivadoM (VivadoM)
import "bittide-extra" Control.Exception.Extra (brackets)

import qualified Bittide.Calculator as Calc
import qualified Bittide.Instances.Hitl.ManticoreDemo.MemoryMaps as MemoryMaps
import qualified Bittide.Instances.Hitl.Utils.OpenOcd as Ocd
import qualified Bittide.Instances.Hitl.WireDemo.Driver as WD
import qualified Clash.Prelude as C
import qualified Clash.Sized.Vector as V
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Text as Text
import qualified Gdb
import qualified System.Timeout.Extra as T

-- ---------------------------------------------------------------------------
-- Manifest (subset of the manticore-runtime manifest.json)
-- ---------------------------------------------------------------------------

{- | One chip's program image + metadata. A single-chip manifest is represented as a
single chip at @(cx,cy)=(0,0)@; a multi-chip (@--chip-dim-x/y@) manifest carries one
'ChipManifest' per @chips[]@ entry, each placed at its grid cell @(cx,cy)@ — which is
also its FPGA node id (@icAt cx cy@).
-}
data ChipManifest = ChipManifest
  { cmCx :: Int
  , cmCy :: Int
  , cmInitializers :: [FilePath]
  , cmProgram :: FilePath
  , cmExceptions :: [(Int, String)]
  -- ^ this chip's own privileged exceptions, (eid, type)
  , cmMemories :: [(Int, Int)]
  {- ^ this chip's reserved global-memory regions, (base, size) words — the image is laid
  out ABOVE them (mirrors run_manifest.py), since user @DefGlobalMemory@ is allocated
  at @userBase + size@ and would otherwise overlap the loaded code.
  -}
  }

data Manifest = Manifest
  { userBase :: Int
  -- ^ reserved global-memory words (the @base@ field, default 0x4000)
  , chips :: [ChipManifest]
  -- ^ one entry (single-chip) or one per @chips[]@ grid cell (multi-chip)
  }

parseManifest :: FilePath -> IO Manifest
parseManifest path = do
  bytes <- Aeson.eitherDecodeFileStrict path
  obj <- case bytes of
    Right (Object o) -> pure o
    Right _ -> fail (path <> ": not a JSON object")
    Left e -> fail (path <> ": " <> e)
  let userBase = fromMaybe 0x4000 (KM.lookup "base" obj >>= asInt)
  cs <- case KM.lookup "chips" obj of
    Just (Array v) -> mapM parseChipVal (foldr (:) [] v)
    _ -> (: []) <$> parseChipObj obj -- single-chip: top-level program/initializers/exceptions
  pure Manifest{userBase, chips = cs}
 where
  asString (String s) = Just (Text.unpack s)
  asString _ = Nothing
  asInt (Number n) = Just (round n)
  asInt _ = Nothing
  itemsOf o k = case KM.lookup k o of Just (Array v) -> foldr (:) [] v; _ -> []
  parseChipVal (Object o) = parseChipObj o
  parseChipVal _ = fail (path <> ": a 'chips' entry is not a JSON object")
  parseChipObj o = do
    prog <- case KM.lookup "program" o >>= asString of
      Just p -> pure p
      Nothing -> fail (path <> ": chip missing 'program'")
    pure
      ChipManifest
        { cmCx = fromMaybe 0 (KM.lookup "cx" o >>= asInt)
        , cmCy = fromMaybe 0 (KM.lookup "cy" o >>= asInt)
        , cmInitializers = mapMaybe asString (itemsOf o "initializers")
        , cmProgram = prog
        , cmExceptions =
            [ (eid, ty)
            | Object e <- itemsOf o "exceptions"
            , Just eid <- [KM.lookup "eid" e >>= asInt]
            , Just ty <- [KM.lookup "type" e >>= asString]
            ]
        , cmMemories =
            [ (base, size)
            | Object m <- itemsOf o "memories"
            , Just base <- [KM.lookup "base" m >>= asInt]
            , Just size <- [KM.lookup "size" m >>= asInt]
            ]
        }

-- ---------------------------------------------------------------------------
-- gmem image layout (mirror of manticore-runtime / run_manifest.py)
-- ---------------------------------------------------------------------------

data Binary = Binary {binName :: String, binBase :: Int, binWords :: [Word16], binIsInit :: Bool}

-- | Read an exec.bin as little-endian 16-bit words.
readBinWords :: FilePath -> IO [Word16]
readBinWords p = packLe . BS.unpack <$> BS.readFile p
 where
  packLe (lo : hi : rest) = (fromIntegral lo .|. (fromIntegral hi `shiftL` 8)) : packLe rest
  packLe _ = []

{- | Lay out one chip's initializers then its main program above the reserved region.
The base is @max(userBase, max over memories of base+size)@ so the loaded code never
overlaps a reserved global-memory region (mirrors run_manifest.py's layout).
-}
layoutChip :: FilePath -> Int -> ChipManifest -> IO [Binary]
layoutChip mdir ubase cm = do
  (initBins, next) <- go ([], startBase) (zip [(0 :: Int) ..] (cmInitializers cm))
  mw <- readBinWords (resolve (cmProgram cm))
  pure (initBins <> [Binary "main" next mw False])
 where
  startBase = maximum (ubase : [b + s | (b, s) <- cmMemories cm])
  resolve p = if take 1 p == "/" then p else mdir </> p
  go acc [] = pure acc
  go (acc, base) ((i, p) : rest) = do
    w <- readBinWords (resolve p)
    go (acc <> [Binary ("init_" <> show i) base w True], base + length w) rest

-- ---------------------------------------------------------------------------
-- schedule_config command encoding (mirror of the Management decode)
-- ---------------------------------------------------------------------------

startCmd :: Word64 -> Word64
startCmd timeout = (1 `shiftL` 63) .|. timeout

resumeCmd :: Word64 -> Word64
resumeCmd timeout = (1 `shiftL` 63) .|. (1 `shiftL` 56) .|. timeout

flushCmd :: Word64
flushCmd = 2 `shiftL` 56

{- | CMD_START_AT: boot, then hold the (independently gated) compute clock until the
reset-aligned @totalCycleCount@ reaches @s@, then run. No @timeout_enabled@ bit.
This is the coordinated simultaneous-start primitive: issue the same @s@ to every
chip and they all ungate together (RTL ungates on @totalCycleCount >= s@).
-}
startAtCmd :: Word64 -> Word64
startAtCmd s = (3 `shiftL` 56) .|. s

{- | Armed CMD_START_AT: + STALL_ARM (bit 55) so the chip gates ONLY on the coordinated
STALL wave (eid 0x7FFF); @$display@/FLUSH is captured but NON-stalling. Used for main.
-}
armedStartAtCmd :: Word64 -> Word64
armedStartAtCmd s = startAtCmd s .|. (1 `shiftL` 55)

mainTimeout, initTimeout :: Word64
mainTimeout = 50_000_000
initTimeout = 1_000_000

{- | @totalCycleCount@ headroom for CMD_START_AT arming. The host arms 8 chips over slow
sequential GDB writes (vs the sim's instant pokes, margin 2000), so @s@ must clear the
full arming + boot spread; otherwise a late-armed chip ungates immediately (RTL @>=@)
and runs out of lockstep. On top of arming, the seam-gated boot (see
'runManticoreMulti': retract extends, boot, settle, verify, re-extend) spends a
~1s wall settle plus ~40 GDB writes inside the margin window, so it is sized in
seconds, not milliseconds. 1e9 cycles = 8s at 125MHz.
-}
startMargin :: Word64
startMargin = 2_500_000_000

{- | Wall-clock settle time between arming the chips of a phase and re-extending
their seams: long enough that every chip has finished its (ms-scale) boot and is
parked in @sResumeWait@ (compute clock gated) — verified afterwards via the
@clock_active@ register rather than by racing the short boot window.
-}
bootSettleMicros :: Int
bootSettleMicros = 1_000_000

{- | Seconds of headroom between reading node 0's local counter and the timed reset-release
(mirrors the wire demo). Must exceed the host's per-node TimedReset.release_cycle bookkeeping
so no chip's local counter has already passed its release cycle when it is written.
-}
type StartDelay = 30

{- | Golden from the manticore-hw @Mips32SimTester@ (interpreter-verified, the
same MIPS32 sum program): it halts via @$finish@ (eid 3) at 53 virtual cycles
after 32 flushes = 31 RF-write displays, and the RF[2] (running-sum) write
values are 'goldenRf2'. We verify the same structure + RF[2] values here.
-}
goldenRf2 :: [Int]
goldenRf2 = [0, 0, 1, 3, 6, 10, 15, 21, 28, 36, 45]

goldenFlushes, goldenDisplays, goldenVcycles, finishEid :: Int
goldenFlushes = 32
goldenDisplays = 31
goldenVcycles = 53
finishEid = 3 -- the MIPS halt $finish (manifest eid 3)

-- ---------------------------------------------------------------------------
-- Driver
-- ---------------------------------------------------------------------------

regAddr :: String -> String -> Integer
regAddr dev reg =
  either (error . (("manticore reg " <> dev <> "." <> reg <> ": ") <>)) id $
    getPathAddress MemoryMaps.managementUnit ["0", dev, reg]

{- | Address of one register of link @k@'s receive ring buffer (duplicate device
names canonicalize to @ReceiveRingBuffer0..6@ in the memory map).
-}
rxRingAddr :: Int -> String -> Integer
rxRingAddr k = regAddr ("ReceiveRingBuffer" <> show k)

{- | Run a GDB action with the MU briefly halted, resuming it afterwards. The MU
firmware keeps running between accesses (it monitors the elastic buffers over
UART during the application phase); RISC-V debug memory access on this rig
requires a halted hart (progbuf; sysbus is disabled), so every host poke/peek
must wrap itself in a halt/resume pair rather than leaving the MU halted.
-}
withHalted :: Gdb.Gdb -> IO a -> IO a
withHalted gdb act = do
  haltSynced gdb
  act `finally` resumeSettled gdb

{- | Halt the target (SIGINT to gdb) and RESYNCHRONIZE the gdb session before
returning: the raw 'Gdb.continue' leaves gdb in foreground execution (no
prompt), and the SIGINT stop banner is printed asynchronously — issuing a
command in that window desyncs gdb-hs's marker framing (observed as "Wait for
magic (start)" timeouts). Settle, then run a synchronized no-op 'echo' (with
one retry) so the stop banner is consumed and the prompt is proven live.
-}
haltSynced :: Gdb.Gdb -> IO ()
haltSynced gdb = do
  Gdb.interrupt gdb
  threadDelay 100_000
  resync (1 :: Int)
 where
  resync retriesLeft = do
    r <- try @SomeException (Gdb.runCommand gdb "echo mu-halt-sync")
    case r of
      Right () -> pure ()
      Left e
        | retriesLeft > 0 -> threadDelay 500_000 >> resync (retriesLeft - 1)
        | otherwise -> throwIO e

{- | Resume the target (raw foreground @continue@; the prompt goes away until
the next halt) and give gdb a moment to process it before the caller moves on.
-}
resumeSettled :: Gdb.Gdb -> IO ()
resumeSettled gdb = do
  Gdb.continue gdb
  threadDelay 20_000

driver ::
  String ->
  [(HwTarget, DeviceInfo)] ->
  VivadoM ExitCode
driver testName targets = do
  v <- ask
  liftIO . putStrLn $ "Manticore demo driver: " <> show (length targets) <> " target(s)"
  forM_ targets (assertProbe "probe_test_start")

  -- Reset USB adapter, see documentation of "Bittide.Instances.Hitl.Utils.Usb".
  liftIO $ forM_ targets $ \(_, d) -> resetUsbDeviceByLocation d.usbAdapterLocation

  projectDir <- liftIO $ findParentContaining "cabal.project"
  let
    hitlDir = projectDir </> "_build/hitl" </> testName
    programDir = projectDir </> "_build/manticore/program"
    expectedJtagIds = [0x0514C001, 0x1514C001, 0x2514C001] -- boot / MU / CC
    toInitArgs (_, deviceInfo) targetIndex =
      Ocd.InitOpenOcdArgs{deviceInfo, expectedJtagIds, hitlDir, targetIndex}
    initArgs = L.zipWith toInitArgs targets [0 ..]
    picocomStarts = liftIO <$> L.zipWith (initPicocom hitlDir) targets [0 ..]

  manifest <- liftIO $ parseManifest (programDir </> "manifest.json")
  liftIO . putStrLn $
    "Manticore image: "
      <> show (length (chips manifest))
      <> " chip(s), userBase "
      <> show (userBase manifest)

  let
    bootInitArgs = L.repeat def{Ocd.logPrefix = "boot-", Ocd.initTcl = "vexriscv_boot_init.tcl"}
    openOcdBootStarts = liftIO <$> L.zipWith Ocd.initOpenOcd initArgs bootInitArgs

  -- Boot CPUs: configure the Si539x clocks (the demo boot firmware), then idle.
  brackets picocomStarts (liftIO . snd) $ \(L.map fst -> picocoms) ->
    brackets openOcdBootStarts (liftIO . (.cleanup)) $ \initOcdsData -> do
      let bootTapInfos = Ocd.parseBootTapInfo <$> initOcdsData
      Gdb.withGdbs (L.length targets) $ \bootGdbs -> do
        liftIO $ zipWithConcurrently3_ (initGdb hitlDir "manticore-demo-boot") bootGdbs bootTapInfos targets
        liftIO $ mapConcurrently_ ((assertEither =<<) . Gdb.loadBinary) bootGdbs
        liftIO $ mapConcurrently_ Gdb.continue bootGdbs
        liftIO $
          T.tryWithTimeout T.PrintActionTime "Waiting for boot done" 60_000_000 $
            forConcurrently_ picocoms $
              \pico -> waitForLine pico "[BT] Going into infinite loop.."

  -- Clock-control + management-unit CPUs: run the generic bring-up firmware on
  -- every node (clock control + link startup + UGN capture + grooming) — the
  -- SAME flow as the wire demo, exercising all the relevant Bittide code. This
  -- brings the Bittide domain up to the stored golden latencies. The MU idles
  -- after grooming; halt it, then poke the chip over GDB.
  let openOcdStarts = liftIO <$> L.zipWith Ocd.initOpenOcd initArgs (L.repeat def)
  brackets openOcdStarts (liftIO . (.cleanup)) $ \initOcdsData -> do
    let
      allTapInfos = Ocd.parseTapInfo expectedJtagIds <$> initOcdsData
      muTapInfos = pick 1 allTapInfos
      ccTapInfos = pick 2 allTapInfos
    Gdb.withGdbs (L.length targets) $ \ccGdbs -> do
      liftIO $
        zipWithConcurrently3_ (initGdb hitlDir "manticore-demo-clock-control") ccGdbs ccTapInfos targets
      liftIO $ mapConcurrently_ ((assertEither =<<) . Gdb.loadBinary) ccGdbs
      Gdb.withGdbs (L.length targets) $ \muGdbs -> do
        liftIO $
          zipWithConcurrently3_ (initGdb hitlDir "manticore-demo-management-unit") muGdbs muTapInfos targets
        liftIO $ mapConcurrently_ ((assertEither =<<) . Gdb.loadBinary) muGdbs

        brackets picocomStarts (liftIO . snd) $ \(L.map fst -> picocoms) -> do
          let
            goDumpCcSamples =
              dumpCcSamples MemoryMaps.clockControl hitlDir (defCcConf (L.length targets)) ccGdbs

          -- Run the bring-up: clock control (keeps running) + link startup + UGN capture.
          liftIO $ mapConcurrently_ Gdb.continue ccGdbs
          liftIO $ mapConcurrently_ Gdb.continue muGdbs

          -- Wait for every MU to capture + print its UGNs.
          liftIO
            $ T.tryWithTimeoutOn
              T.PrintActionTime
              "Waiting for captured UGNs"
              60_000_000
              goDumpCcSamples
            $ forConcurrently_ picocoms
            $ \pico -> waitForLine pico "[MU] Printed all hardware UGNs"

          -- Read the captured UGNs from EVERY target over GDB (the chip's CaptureUgn
          -- MMIO, halted). This is the milestone: all rig links brought up and all
          -- UGNs collected.
          liftIO $ putStrLn "Getting UGNs for all targets"
          liftIO $ mapConcurrently_ Gdb.interrupt muGdbs
          ugnPairsTable <-
            liftIO $ zipWithConcurrently (WD.readHardwareUgns MemoryMaps.managementUnit) targets muGdbs
          let
            ugnPairsTableV = fromJust . V.fromList $ fromJust . V.fromList <$> ugnPairsTable
          liftIO $ do
            putStrLn "Calculating IGNs for all targets"
            Calc.printAllIgns ugnPairsTableV fpgaSetup
            putStrLn "UGN pairs table:"
            mapM_ print ugnPairsTableV

          -- Groom this boot onto the stored golden latencies (proves the rig can be
          -- relabeled, mirroring the wire demo): Bellman-Ford relabel + per-link
          -- elastic-buffer corrections.
          let measuredEdges = hardwareUgnEdges ugnPairsTableV
          RelabelPlan{resetOffsets, corrections = correctionsPerNode} <-
            case computeRelabel measuredEdges WD.lambdaSafe of
              Left ns ->
                fail $
                  "UGN grooming infeasible (UGNs changed too much); negative cycle through nodes: "
                    <> show ns
              Right plan -> pure plan
          liftIO $ do
            putStrLn $
              "Grooming onto stored lambda^safe ("
                <> show (L.length WD.lambdaSafe)
                <> " edges, margin "
                <> show WD.marginFrames
                <> ")"
            putStrLn "Per-node frame corrections (per link):"
            mapM_ print (V.toList correctionsPerNode)

          -- Apply the elastic-buffer corrections on every node.
          liftIO $ do
            putStrLn "\n=== Applying elastic-buffer corrections ==="
            mapConcurrently_
              (\(gdb, corr) -> writeCorrections MemoryMaps.managementUnit gdb corr)
              (L.zip muGdbs (V.toList correctionsPerNode))
            mapConcurrently_ Gdb.continue muGdbs
            T.tryWithTimeoutOn
              T.PrintActionTime
              "Waiting for corrections to be applied"
              60_000_000
              goDumpCcSamples
              $ forConcurrently_ picocoms
              $ \pico -> waitForLine pico "[MU] Corrections applied successfully"

          -- NB: no CC-sample dump and no MU halt here. 'dumpCcSamples' HALTS the
          -- clock-control CPUs (GDB memory access needs a halted hart) and nothing
          -- resumes them -- dumping mid-flow would leave Callisto DEAD for the whole
          -- application phase, letting the clocks free-run and the (no longer
          -- auto-centered) elastic buffers rail within seconds: every seam link then
          -- freezes on a stale word. Exactly this took the multi-chip seams down.
          -- The MU likewise keeps running from here (its firmware now monitors the
          -- elastic buffers over UART); every subsequent MU-GDB access below halts
          -- it briefly via 'withHalted' and resumes it. Samples are dumped once at
          -- the very end, like the wire demo.
          liftIO $ putStrLn "=== UGNs collected + groomed on all targets ==="

          -- Reset-aligned start. appReset (wired into the Manticore chip's reset in UserCore.hs)
          -- defaults HELD (TimedReset.release_cycle = maxBound), so we MUST release it here or
          -- the chip never leaves reset -- this is required even single-chip. Releasing each
          -- node at sharedBase + its relabel offset gives every chip's free-running
          -- totalCycleCount a common, boot-skew-absorbed time origin across FPGAs: resetOffsets
          -- is the Bellman-Ford potential gauged to node 0, and the staggered release makes the
          -- per-chip starts consistent with the groomed seam latencies (mirrors the wire demo).
          -- The per-chip CMD_START_AT in runManticoreMulti then lands the chips in lockstep.
          currentTime <-
            liftIO $
              withHalted (L.head muGdbs) $
                readCurrentTime MemoryMaps.managementUnit (L.head targets) (L.head muGdbs)
          let
            sharedBase = currentTime + C.natToNum @(C.PeriodToCycles GthTx (C.Seconds StartDelay))
            tResets :: C.Vec FpgaCount (C.Unsigned 64)
            tResets =
              C.map (\off -> C.checkedFromIntegral (toInteger sharedBase + toInteger off)) resetOffsets
          liftIO $ do
            putStrLn $
              "=== Reset-aligned release: sharedBase="
                <> show sharedBase
                <> " (node0 now + "
                <> show (C.natToNum @StartDelay :: Integer)
                <> "s) ==="
            putStrLn "Per-node tReset (= sharedBase + relabel offset):"
            mapM_ print (V.toList tResets)
            forM_ (L.zip muGdbs (V.toList tResets)) $ \(gdb, tReset) ->
              withHalted gdb $ writeReleaseCycle MemoryMaps.managementUnit gdb tReset
          -- Confirm no node's local counter has already passed its tReset (host bookkeeping fit
          -- inside the StartDelay headroom), else that node would release before it is set up.
          releaseInTime <-
            liftIO $ forM (L.zip3 targets muGdbs (V.toList tResets)) $ \(tgt, gdb, tReset) -> do
              now <- withHalted gdb $ readCurrentTime MemoryMaps.managementUnit tgt gdb
              pure (now < tReset)
          liftIO $
            unless (L.and releaseInTime) $
              fail "Reset-release window missed: setup took longer than the StartDelay margin"
          -- Wait for appReset to release on every node (local counter passes tReset). Now each
          -- chip's totalCycleCount counts from the shared aligned origin and the chip is live
          -- for image load + CMD_START_AT.
          liftIO $ do
            let startDelaySec = C.natToNum @StartDelay :: Int
                delayMicros = (startDelaySec + 1) * 1_000_000
            putStrLn $ "Waiting ~" <> show (startDelaySec + 1) <> "s for the reset-aligned release..."
            threadDelay delayMicros
          liftIO $ do
            -- Arm the seam ILA on EVERY FPGA. Each FPGA is a separate Vivado hw target, and
            -- 'get_hw_ilas' only sees the currently-open target's core -- so we must
            -- 'openHwTarget' each one before arming, exactly as the ILA-readback loop does.
            -- (Previously we armed only the last-opened target, leaving 7 of 8 ILAs unarmed,
            -- so the readback found every core empty -> zero samples.)
            putStrLn "Verifying + arming seam ILAs on all targets..."
            forM_ targets $ \(hwT, _) -> do
              openHwTarget v hwT
              refresh_hw_device v ["-quiet"]
              ilas <- get_hw_ilas v []
              unless (null ilas) $ putStrLn [i|  arming ilas #{ilas}|]
              forM_ ilas $ \ila -> do
                _ <- current_hw_ila v [show ila]
                -- Trigger probe (active-high): arm on the first seam frame.
                let triggerProbe = "[get_hw_probes -of_objects [current_hw_ila] */trigger*]"
                execCmd_ v "set_property" ["trigger_compare_value", "eq1'b1", triggerProbe]
                -- Capture control: store only cycles where the capture probe is high.
                execCmd_ v "set_property" ["control.capture_mode", "BASIC", "[current_hw_ila]"]
                let captureProbe = "[get_hw_probes -of_objects [current_hw_ila] */capture*]"
                execCmd_ v "set_property" ["capture_compare_value", "eq1'b1", captureProbe]
                execCmd_ v "set_property" ["control.trigger_position", "0", "[current_hw_ila]"]
                run_hw_ila v ["[current_hw_ila]"]
          -- Run the Manticore program. Single-chip: the board-verified runManticore on the
          -- head target. Multi-chip (--chip-dim-x/y): load each chip's split image into its
          -- OWN FPGA, configure the per-FPGA seams (grid mesh), and run them as one folded torus
          -- over the seams with a reset-aligned, armed CMD_START_AT start (now that the timed
          -- reset above gave every chip a common totalCycleCount origin) + run-to-FINISH.
          exitCode <- case chips manifest of
            [single] -> liftIO $ do
              bins <- layoutChip programDir (userBase manifest) single
              case zip muGdbs targets of
                [] -> pure ExitSuccess
                ((gdb, _) : _) -> runManticore gdb bins (cmExceptions single)
            cs -> liftIO $ do
              -- Seam-latency sweep (debug): if the compile step produced
              -- sweep_* image sets, run EVERY set in this one rig session
              -- (same bitstream, images differ only in the seam-latency CSV)
              -- and report a per-set verdict matrix. Otherwise the plain run.
              sweeps <- findSweepSets programDir
              if null sweeps
                then runManticoreMulti programDir (userBase manifest) cs (L.zip [0 ..] muGdbs)
                else
                  runManticoreSweep
                    (("baseline", programDir) : sweeps)
                    (L.zip [0 ..] muGdbs)
          -- Dump the clock-control samples LAST (mirrors the wire demo): this halts
          -- the CC CPUs, which is only safe once the application run is over.
          liftIO goDumpCcSamples
          pure exitCode
 where
  pick i xss = [xs !! i | xs <- xss, length xs > i]

{- | Single-chip run. The MU is halted for the whole run (one halt/resume pair);
its elastic-buffer monitor only matters for the multi-chip seams anyway.
-}
runManticore :: Gdb.Gdb -> [Binary] -> [(Int, String)] -> IO ExitCode
runManticore gdb bins excMap = withHalted gdb $ do
  let
    aSched = regAddr "ManticoreControl" "schedule_config"
    aGmem = regAddr "ManticoreControl" "gmem_base"
    aTrace = regAddr "ManticoreControl" "trace_base"
    aStart = regAddr "ManticoreControl" "start"
    aEid = regAddr "ManticoreControl" "exception_id"
    aVc = regAddr "ManticoreControl" "virtual_cycles"
    aDone = regAddr "ManticoreControl" "done"
    -- Base byte address of the memory-mapped gmem region on the MU bus. The
    -- region is 32-bit-word addressable with byte write-enables; gmem 16-bit
    -- half-word @h@ lives at byte offset @h * 2@ (little-endian within each
    -- 32-bit word, matching the chip's GmemHalfWordAdapter).
    aGmemRegion = regAddr "ManticoreGmem" "data"

    poke64 :: Integer -> Word64 -> IO ()
    poke64 a v = Gdb.writeLe gdb a v
    peek32 :: Integer -> IO Int
    peek32 a = fromIntegral <$> (Gdb.readLe gdb a :: IO Word32)
    peek64 :: Integer -> IO Int
    peek64 a = fromIntegral <$> (Gdb.readLe gdb a :: IO Word64)

    -- Bulk-load a contiguous run of 16-bit gmem words starting at half-word
    -- offset @hwBase@, in a single GDB `restore` (one round-trip, not one per
    -- word). Empty runs are skipped.
    restoreWords :: Int -> [Word16] -> IO ()
    restoreWords _ [] = pure ()
    restoreWords hwBase ws = do
      let
        bytes = BS.pack (concatMap (\w -> [fromIntegral w, fromIntegral (w `shiftR` 8)]) ws)
        addr = aGmemRegion + fromIntegral (hwBase * 2)
        path = "/tmp/manticore_gmem_" <> show hwBase <> ".bin"
      BS.writeFile path bytes
      Gdb.runCommand gdb ("restore " <> path <> " binary 0x" <> showHex addr "")

    showHex32 :: Int -> String
    showHex32 x = "0x" <> showHex x ""

    -- Read a 32-bit gmem word at byte offset @byteOff@ in the region.
    gmemRead32 :: Int -> IO Int
    gmemRead32 byteOff =
      fromIntegral <$> (Gdb.readLe gdb (aGmemRegion + fromIntegral byteOff) :: IO Word32)

    -- Decode the whole trace record (mirrors run_manifest.py read_trace): the
    -- design writes the latest $display at gmem words 0..3 (trace_base = 0).
    --   w0 = pc, w1 = instr, w2 = (rg | val_lo<<16), w3 = (val_hi | ...).
    -- Returns (pc, instr, rg, val) for instrumentation.
    readTraceRecord :: IO (Int, Int, Int, Int)
    readTraceRecord = do
      w0 <- gmemRead32 0
      w1 <- gmemRead32 4
      w2 <- gmemRead32 8
      w3 <- gmemRead32 12
      let
        rg = w2 .&. 0xffff
        val = ((w2 `shiftR` 16) .&. 0xffff) .|. ((w3 .&. 0xffff) `shiftL` 16)
      pure (w0, w1, rg, val)

    classify eid
      | eid > (0xFFFF :: Int) = "TIMEOUT"
      | otherwise = fromMaybe "unknown" (lookup eid excMap)

    -- Poll the clean command-complete flag (the user core's cmdComplete FSM),
    -- which is True only once the command issued by the latest `start` has
    -- actually run to completion — so it is safe against sampling the previous
    -- command's stale done/idle.
    waitDone = go (0 :: Int)
     where
      go n
        | n > 5000 = fail "Manticore: timeout waiting for command completion"
        | otherwise = do
            d <- peek32 aDone
            when (d == 0) (threadDelay 2_000 >> go (n + 1))

    runCmd name base cmd = do
      poke64 aSched cmd
      poke64 aGmem (fromIntegral base)
      poke64 aStart 1
      waitDone
      eid <- peek32 aEid
      vc <- peek64 aVc
      let k = classify eid
      putStrLn $ "  " <> name <> " eid=" <> show eid <> " vcycles=" <> show vc <> " -> " <> k
      pure (eid, k)

  putStrLn "Loading Manticore image into the gmem region (bulk restore)..."
  forM_ bins $ \b -> restoreWords (binBase b) (binWords b)

  poke64 aTrace 0

  forM_ (filter binIsInit bins) $ \b -> runCmd (binName b) (binBase b) (startCmd initTimeout)
  let baseM = binBase (last bins)
  (eidMain, _) <- runCmd "main" baseM (startCmd mainTimeout)

  -- FLUSH/resume loop, mirroring Mips32SimTester.run: on each FLUSH stop issue
  -- a cache-flush (pushes the $display record to gmem), then — only for an
  -- RF-write display (eid 1), not the eid-2 "Got halt!" — read and record the
  -- trace; resume; repeat until the program halts (eid 3, FINISH).
  let
    runRaw cmd = do
      poke64 aSched cmd
      poke64 aGmem (fromIntegral baseM)
      poke64 aStart 1
      waitDone
      peek32 aEid
    loop eid flushes recs
      | classify eid == "FLUSH" && flushes < (400 :: Int) = do
          let flushEid = eid
          _ <- runRaw flushCmd
          recs' <-
            if flushEid == 1
              then do
                rec@(pc, instr, rg, val) <- readTraceRecord
                putStrLn $
                  "  flush#"
                    <> show flushes
                    <> " "
                    <> showHex32 pc
                    <> " "
                    <> showHex32 instr
                    <> ": RF["
                    <> show rg
                    <> "] <= "
                    <> show val
                pure (recs <> [rec])
              else do
                putStrLn $ "  flush#" <> show flushes <> " (eid=" <> show flushEid <> ": 'Got halt!')"
                pure recs
          eid' <- runRaw (resumeCmd mainTimeout)
          loop eid' (flushes + 1) recs'
      | otherwise = pure (eid, flushes, recs)
  (finalEid, flushes, recs) <- loop eidMain (0 :: Int) []
  vcFinal <- peek64 aVc

  let
    rf2 = [v | (_, _, rg, v) <- recs, rg == 2]
    nDisplays = length recs
    structOk = finalEid == finishEid && flushes == goldenFlushes && nDisplays == goldenDisplays
    valOk = rf2 == goldenRf2
  putStrLn $
    "=== Manticore RESULT: eid="
      <> show finalEid
      <> " ("
      <> classify finalEid
      <> "), "
      <> show flushes
      <> " flushes, "
      <> show nDisplays
      <> " RF-write displays, "
      <> show vcFinal
      <> " vcycles ==="
  putStrLn $ "  RF[2] values: " <> show rf2
  putStrLn $ "  golden RF[2]: " <> show goldenRf2
  putStrLn $
    "  structural golden: eid="
      <> show finishEid
      <> " flushes="
      <> show goldenFlushes
      <> " displays="
      <> show goldenDisplays
      <> " vcycles="
      <> show goldenVcycles
      <> (if vcFinal == goldenVcycles then " (vcycles match)" else " (vcycles DIFFER)")
  if structOk && valOk
    then
      putStrLn "PASS: exact match to interpreter golden (structure + RF[2] values)" >> pure ExitSuccess
    else putStrLn "FAIL: trace does not match golden" >> pure (ExitFailure 1)

-- ---------------------------------------------------------------------------
-- Multi-chip distributed run
-- ---------------------------------------------------------------------------

{- | Golden for the @loop_multi@ 8x16 torus, from the interpreter / 'MultiChipTdmSimTester'
(structurally verified in RTL sim): the reporter chip @(0,0)@ reaches FINISH around 1025
virtual cycles (plus the stall-wave tail of diameter+margin extra vcycles). Reported, not
asserted on the exact value (the armed run stops on the coordinated wave, not per-$display).
-}
goldenMultiVcycles :: Int
goldenMultiVcycles = 1025

{- | Final seam-crossed @sig2@ (the third @$display@ argument) after the last SIG of the
@loop_multi@ run. It is the fully-folded signature whose value only comes out right when the
inter-chip NoC delivered every fold across the chip-to-chip seams — so asserting it makes the
multi-chip pass DATA-DEPENDENT rather than mere liveness (the reporter's @$finish@ fires on a
fixed cycle count regardless of received data). @sig0@/@sig1@ depend on the same cross-seam
transport and are reported, not asserted (the full triple is asserted once a green rig run
confirms; the 8-chip SIM now reaches the full golden (225,225,225) after the compiler's
NoC reservation-anchoring fix — see manticore-compiler 2b203f9 and SEAM-RESIDUAL.md).
Interpreter golden (20/96/193/225); the last is 225.
-}
goldenFinalSig2 :: Int
goldenFinalSig2 = 225

{- | Final record of the second @$display@ statement (\"CHK %d %d %d\", manifest eid 2, trace
words [6][7][8,9]): an 8-bit +3 counter, a 16-bit LFSR and a 32-bit accumulator — three
distinct state registers with different update rules and widths, all LOCAL to the reporter's
privileged process. Their values never cross a chip seam, so this asserts the multi-state,
multi-statement @$display@\/trace mechanism itself (guest state -> GST trace words -> host
readback) end-to-end on the rig, independently of the cross-seam application values.
Interpreter golden of the last CHK (guest cycle 895; the display reads the states'
next-value wires, which co-locates the update logic with the reporter process): counter
128, LFSR 11707, acc 115475.
-}
goldenFinalChk :: (Int, Int, Int)
goldenFinalChk = (128, 11707, 115475)

{- | Write FPGA node @node@'s inter-chip seam configuration over its (halted) MU gdb: per
torus edge, the @seam_<edge>_extend@ bit and (when extended) the @seam_<edge>_link@
Bittide-link index, then raise @seam_enable@ to switch those links from forwarding the
handshake to driving the chip's TDM seam frames. See 'Latencies.seamConfig' (the grid
mesh: an edge extends iff a grid neighbour exists; the link reaches that neighbour FPGA).
-}
configureSeams :: Gdb.Gdb -> Int -> IO ()
configureSeams gdb node = do
  putStrLn $
    "  node "
      <> show node
      <> " seams: "
      <> show [(dirName d, ext, ml) | (d, ext, ml) <- seamConfig node]
  forM_ (seamConfig node) $ \(d, ext, ml) -> do
    let edge = dirName d
    Gdb.writeLe
      gdb
      (regAddr "UserConfig" ("seam_" <> edge <> "_extend"))
      (if ext then 1 else 0 :: C.BitVector 32)
    forM_ ml $ \lk ->
      Gdb.writeLe
        gdb
        (regAddr "UserConfig" ("seam_" <> edge <> "_link"))
        (fromIntegral lk :: C.Index LinkCount)
  -- raise seam_enable last so the extend/link config is in place before the switch
  Gdb.writeLe gdb (regAddr "UserConfig" "seam_enable") (1 :: C.BitVector 32)

{- | Set every topologically-wired seam edge's @extend@ bit of one chip to the given
value (retract to @False@, re-extend to @True@); unwired edges stay retracted. With
@extend = 0@ a chip edge U-turns inside the chip — exactly the per-chip boot topology
the split images are built for — and the boundary bridge is bypassed, so boot NoC
frames cannot leak across a seam and any frames left frozen in the bridge by a
previous (stall-gated) run drain into the disconnected demux instead of the booting
cores. Mirrors the sim kernel's boot gating ('MultiChipPerMgmtSimKernel', which
fixed exactly these two failure modes in simulation).
-}
setSeamExtends :: Gdb.Gdb -> Int -> Bool -> IO ()
setSeamExtends gdb node on =
  -- Only the topologically-wired edges are touched: unwired ones are 0 and stay
  -- 0, and every write is a slow (~150ms) GDB round trip inside the arming margin.
  forM_ [d | (d, ext, _) <- seamConfig node, ext] $ \d ->
    Gdb.writeLe
      gdb
      (regAddr "UserConfig" ("seam_" <> dirName d <> "_extend"))
      (if on then 1 else 0 :: C.BitVector 32)

{- | Seam-latency sweep image sets produced by @manticore_compile_program.sh@
(@MANTICORE_SWEEP_DELTAS@): the @sweep_*@ subdirectories of the program dir that
contain a manifest, in name order (the script prefixes a run-order index).
-}
findSweepSets :: FilePath -> IO [(String, FilePath)]
findSweepSets dir = do
  entries <- either (\(_ :: SomeException) -> []) id <$> try (listDirectory dir)
  let candidates = L.sort [e | e <- entries, "sweep_" `L.isPrefixOf` e]
  fmap concat $ forM candidates $ \e -> do
    hasManifest <- doesFileExist (dir </> e </> "manifest.json")
    pure [(e, dir </> e) | hasManifest]

{- | Run every image set in one rig session and report a per-set verdict matrix.
The chips accept a fresh image load + coordinated start after a (stall-gated)
FINISH — each phase is a full re-boot from gmem, exactly like the proven
init0/init1/main sequence — so one HITL run can test several seam-latency
hypotheses back to back. A set that throws (e.g. a command-completion timeout)
is reported and does not abort the remaining sets. The sweep passes iff ANY
set passes, and the matrix names the winner(s).
-}
runManticoreSweep :: [(String, FilePath)] -> [(Int, Gdb.Gdb)] -> IO ExitCode
runManticoreSweep sets nodeGdbs = do
  putStrLn $ "=== SEAM-LATENCY SWEEP: " <> show (length sets) <> " image set(s) ==="
  results <- forM sets $ \(label, dir) -> do
    putStrLn $ "=== sweep set '" <> label <> "' (" <> dir <> ") ==="
    r <- try @SomeException $ do
      mf <- parseManifest (dir </> "manifest.json")
      runManticoreMulti dir (userBase mf) (chips mf) nodeGdbs
    case r of
      Left err -> do
        putStrLn $ "  sweep set '" <> label <> "' EXCEPTION: " <> show err
        pure (label, False)
      Right ec -> pure (label, ec == ExitSuccess)
  putStrLn "=== SWEEP VERDICT MATRIX (per seam-latency delta) ==="
  forM_ results $ \(label, ok) ->
    putStrLn $ "  " <> label <> ": " <> (if ok then "PASS (sig2 golden)" else "fail")
  pure (if any snd results then ExitSuccess else ExitFailure 1)

{- | Distributed multi-chip run. Each chip's split image is loaded into ITS OWN FPGA's
gmem (after the caller's timed-reset release gave every chip's totalCycleCount a common
origin), the per-FPGA seams are configured (grid mesh), and the chips run the one
folded-torus program, communicating over the seams.

Each initializer phase and the main are started with a coordinated @CMD_START_AT@ (per
phase: read every chip's @execution_cycles@, pick a common future S = max + margin, and
issue @CMD_START_AT S@ to all chips so they ungate together). Main is ARMED (STALL_ARM):
the chips gate ONLY on the coordinated stall wave (eid 0x7FFF), so @$display@/FLUSH is
captured but NON-stalling, and all chips run in lockstep until the app's @$finish@ trips
the stall wave and halts them together. The reporter chip @(0,0)@ surfaces FINISH, which
the host polls. Per-@$display@ value capture is postponed (it would require stalling).
-}
runManticoreMulti :: FilePath -> Int -> [ChipManifest] -> [(Int, Gdb.Gdb)] -> IO ExitCode
runManticoreMulti programDir ubase cms nodeGdbs = do
  putStrLn $ "=== Multi-chip run: " <> show (length cms) <> " chips over the folded torus ==="
  runs <- forM cms $ \cm -> do
    let node = icAt (cmCx cm) (cmCy cm)
    gdb <- case lookup node nodeGdbs of
      Just g -> pure g
      Nothing ->
        fail $
          "multi-chip run: no MU gdb for chip ("
            <> show (cmCx cm)
            <> ","
            <> show (cmCy cm)
            <> ") = node "
            <> show node
    bins <- layoutChip programDir ubase cm
    pure (node, gdb, bins, cmExceptions cm)

  let
    aSched = regAddr "ManticoreControl" "schedule_config"
    aGmem = regAddr "ManticoreControl" "gmem_base"
    aTrace = regAddr "ManticoreControl" "trace_base"
    aStart = regAddr "ManticoreControl" "start"
    aEid = regAddr "ManticoreControl" "exception_id"
    aVc = regAddr "ManticoreControl" "virtual_cycles"
    aDone = regAddr "ManticoreControl" "done"
    aGmemRegion = regAddr "ManticoreGmem" "data"

    -- The MU firmware monitors the elastic buffers whenever its CPU runs; the host
    -- batches its GDB work into a few COARSE halted regions (seam config + image
    -- load + init phases; the post-FINISH readback), so the monitor is live during
    -- the long windows that matter — above all the armed MAIN run. gdb-hs's
    -- continue/interrupt are raw (no prompt synchronization), so per-operation
    -- halt/resume ping-pong desyncs the session; coarse regions + 'haltSynced'
    -- avoid that.
    muGdbsAll = [g | (_, g, _, _) <- runs]
    haltAll = forM_ muGdbsAll haltSynced
    resumeAll = forM_ muGdbsAll resumeSettled

    poke64 g a v = Gdb.writeLe g a (v :: Word64)
    peek32 g a = fromIntegral <$> (Gdb.readLe g a :: IO Word32) :: IO Int
    peek64 g a = fromIntegral <$> (Gdb.readLe g a :: IO Word64) :: IO Int

    -- Bulk-load a chip's gmem run (per-node temp file so concurrent chips don't clobber).
    restoreWords g node hwBase ws
      | null ws = pure ()
      | otherwise = do
          let
            bytes = BS.pack (concatMap (\w -> [fromIntegral w, fromIntegral (w `shiftR` 8)]) ws)
            addr = aGmemRegion + fromIntegral (hwBase * 2)
            path = "/tmp/manticore_gmem_" <> show node <> "_" <> show hwBase <> ".bin"
          BS.writeFile path bytes
          Gdb.runCommand g ("restore " <> path <> " binary 0x" <> showHex addr "")

    classifyWith exc eid
      | eid > (0xFFFF :: Int) = "TIMEOUT"
      | otherwise = fromMaybe "unknown" (lookup eid exc)

    -- Bound sized for the seam-gated phases: the ungate cycle S sits a full
    -- 'startMargin' (8s) past arming, and completion can only be observed after it.
    waitDone g = go (0 :: Int)
     where
      go n
        | n > 15_000 = fail "Manticore multi: timeout waiting for command completion"
        | otherwise = do
            d <- peek32 g aDone
            when (d == 0) (threadDelay 2_000 >> go (n + 1))

    -- Pick a common future start cycle S for CMD_START_AT. Every chip's free-running
    -- totalCycleCount (the execution_cycles register) is reset-aligned, so S = max + margin
    -- lets them all ungate together (the RTL ungates on totalCycleCount >= S; a chip armed
    -- after S has already passed runs immediately, hence the generous margin).
    aExec = regAddr "ManticoreControl" "execution_cycles"
    aCa = regAddr "ManticoreControl" "clock_active"
    computeStartAt margin = do
      execs <- forM runs $ \(_, gdb, _, _) -> peek64 gdb aExec
      pure (fromIntegral (maximum execs) + margin :: Word64)

    -- Seam-gated coordinated phase start: boot every chip with its seams RETRACTED
    -- (extend = 0: edges U-turn, the per-chip boot topology the split images are
    -- built for), then re-extend once every chip is parked in sResumeWait, so the
    -- seams come up clean before the common ungate at S. Without this, boot NoC
    -- frames leak across the live seams and frames left frozen in the boundary
    -- bridges by a previous stall-gated run thaw into the booting cores — the sim
    -- kernel gates exactly this way ('MultiChipPerMgmtSimKernel' boot gating; its
    -- absence in the sim caused imem body-truncation and boot-frame gate
    -- violations, and on the rig a reload-boot after an armed run hangs).
    --
    -- Sequencing: retract everywhere; arm every chip (the caller's pokes, ending in
    -- the start pulse); settle a fixed wall delay (boots are ms-scale, so no racy
    -- mid-boot polling); assert every chip is booted-and-gated (clock_active low —
    -- it is high through sCoreReset..sBoot and low in sResumeWait); assert S is
    -- still comfortably in the future; re-extend. The startMargin is sized for all
    -- of this (see 'startMargin').
    -- Every GDB round trip costs ~150ms on the rig, so the per-chip batches run
    -- concurrently (each chip has its own GDB session, exactly like the image
    -- load) and the margin bookkeeping is sized from measured costs (run
    -- 28802685894 burned ~6.6s on sequential ops with an 8s margin).
    startPhaseGated :: String -> (Word64 -> IO ()) -> IO Word64
    startPhaseGated label pokes = do
      forConcurrently_ runs $ \(node, gdb, _, _) -> setSeamExtends gdb node False
      s <- computeStartAt startMargin
      putStrLn $ "  " <> label <> ": CMD_START_AT S=" <> show s <> " (seam-gated boot)"
      pokes s
      threadDelay bootSettleMicros
      forConcurrently_ runs $ \(node, gdb, _, _) -> do
        ca <- peek32 gdb aCa
        when (ca /= 0) $
          fail $
            "node " <> show node <> " not gated in sResumeWait after boot settle (" <> label <> ")"
      nowExec <- computeStartAt 0
      when (nowExec > s - 500_000_000) $
        fail $
          label
            <> ": start cycle S too close after boot settle (S="
            <> show s
            <> ", now="
            <> show nowExec
            <> ")"
      forConcurrently_ runs $ \(node, gdb, _, _) -> setSeamExtends gdb node True
      pure s

  (rNode, rGdb, rBins, rExc) <- case [r | r@(n, _, _, _) <- runs, n == 0] of
    (r : _) -> pure r
    [] -> fail "multi-chip run: no reporter chip (0,0) in the manifest"

  -- Coarse halted region: seam config, image load, and the (short) init phases.
  -- The MUs' elastic-buffer monitors pause here and run again for the main phase.
  haltAll

  -- 1. Per-FPGA seam config. 2. Load each chip's split image (concurrently).
  putStrLn "Configuring per-FPGA seams (grid mesh)..."
  forM_ runs $ \(node, gdb, _, _) -> configureSeams gdb node
  -- RX ring-buffer liveness taps (diagnostics): every link's receive ring
  -- buffer continuously records the raw post-EB RX words once enabled. The
  -- post-run window (the buffers hold the last ~32us) distinguishes by VALUE:
  -- evolving words = live handshake link; a constant TDM-frame/zero word =
  -- live seam link frozen by the stall gate; a constant HANDSHAKE-era word =
  -- the link's RX died before main — the seam ILA caught node 0's link 1 in
  -- exactly that state (EB output word, datacount and status flags all frozen,
  -- EBMON silent: a frozen counter never crosses a watermark).
  putStrLn "Enabling RX ring-buffer liveness taps on all links..."
  forConcurrently_ runs $ \(_, gdb, _, _) ->
    forM_ [0 .. 6 :: Int] $ \k ->
      Gdb.writeLe gdb (rxRingAddr k "enable") (1 :: Word8)
  putStrLn "Loading per-chip split images into each FPGA's gmem..."
  forConcurrently_ runs $ \(node, gdb, bins, _) -> do
    forM_ bins $ \b -> restoreWords gdb node (binBase b) (binWords b)
    poke64 gdb aTrace 0

  -- 3. Run each chip's initializers, COORDINATED per phase (CMD_START_AT aligned start) so
  -- any cross-chip init traffic stays in lockstep and each seam empties on a shared vcycle
  -- boundary (mirrors the verified MultiChipPerMgmtSimTester startAtPhase). All chips share
  -- one program, so they have the same init phase count.
  let nInit = case runs of
        ((_, _, bs, _) : _) -> length (filter binIsInit bs)
        [] -> 0
  putStrLn $ "Running " <> show nInit <> " coordinated initializer phase(s) (CMD_START_AT)..."
  forM_ [0 .. nInit - 1] $ \ph -> do
    _ <- startPhaseGated ("init phase " <> show ph) $ \s ->
      forConcurrently_ runs $ \(_, gdb, bins, _) -> do
        let b = filter binIsInit bins !! ph
        poke64 gdb aSched (startAtCmd s)
        poke64 gdb aGmem (fromIntegral (binBase b))
        poke64 gdb aStart 1
    forM_ runs $ \(node, gdb, _, _) -> do
      waitDone gdb
      eid <- peek32 gdb aEid
      when (eid > 0xFFFF) $
        fail $
          "node " <> show node <> " init phase " <> show ph <> " timed out (eid=" <> show eid <> ")"

  -- 4. Armed coordinated start of main (CMD_START_AT + STALL_ARM): every chip gates ONLY on
  -- the coordinated STALL wave (eid 0x7FFF), so $display/FLUSH is captured but NON-stalling.
  -- All chips run in lockstep from the aligned S until the app's $finish trips the stall wave
  -- and halts them together. (Per-$display value capture is postponed.)
  putStrLn "Starting main on all chips (armed CMD_START_AT)..."
  _ <- startPhaseGated "main (ARMED)" $ \sMain ->
    forConcurrently_ runs $ \(_, gdb, bins, _) -> do
      poke64 gdb aSched (armedStartAtCmd sMain)
      poke64 gdb aGmem (fromIntegral (binBase (last bins)))
      poke64 gdb aStart 1

  -- End of the halted region: every MU runs (and monitors its elastic buffers)
  -- for the entire armed main run. The CMD_START_AT margin absorbs the resume
  -- time, so the chips still ungate together at S.
  resumeAll

  -- 5. Armed run: poll the reporter's exception id until the app FINISH. The application eid
  -- is captured separately from the STALL gate, so the reporter surfaces FINISH here once the
  -- coordinated wave has halted everyone. Each poll briefly halts ONLY the reporter's MU
  -- (haltSynced/withHalted); slow pacing keeps its monitor mostly live. ~6 min bound.
  let pollFinish n
        | n > (720 :: Int) = withHalted rGdb $ peek32 rGdb aEid
        | otherwise = do
            e <- withHalted rGdb $ peek32 rGdb aEid
            if classifyWith rExc e == "FINISH" || e > 0xFFFF
              then pure e
              else threadDelay 500_000 >> pollFinish (n + 1)
  finalEid <- pollFinish (0 :: Int)

  -- Post-FINISH readback: one more coarse halted region for the flush + trace +
  -- terminal-eid reads.
  haltAll
  vcFinal <- peek64 rGdb aVc

  -- 5b. Recover the reporter's FINAL $display (SIG) record so the pass is DATA-DEPENDENT, not just
  -- liveness. In armed mode the $display GSTs land in the (write-back) cache but are never flushed
  -- during the run; the reporter is now halted with its last SIG line still dirty in cache. Issue
  -- ONE cache-flush (cmd 2) to drain it to the gmem trace region, then read words 0..5 = the three
  -- 32-bit little-endian signatures sig0/sig1/sig2 (trace_base = 0). Mirrors the verified
  -- MultiChipPerMgmtSimTester flush-after-halt. The flush must precede any re-boot of the reporter
  -- (the cache is not reset between halt and flush) — it is the last thing we do to that chip.
  poke64 rGdb aSched flushCmd
  poke64 rGdb aGmem (fromIntegral (binBase (last rBins)))
  poke64 rGdb aStart 1
  waitDone rGdb
  let gmemRead32 :: Int -> IO Int
      gmemRead32 byteOff =
        fromIntegral <$> (Gdb.readLe rGdb (aGmemRegion + fromIntegral byteOff) :: IO Word32)
  sig0 <- gmemRead32 0
  sig1 <- gmemRead32 4
  sig2 <- gmemRead32 8
  -- Second $display statement ("CHK", eid 2): trace words 6..9 = bytes 12..19. The
  -- compiler numbers trace offsets globally across statements, so both statements'
  -- final records coexist in gmem and are independently readable here.
  chkCntLfsr <- gmemRead32 12
  chkAcc <- gmemRead32 16
  let chkFinal = (chkCntLfsr .&. 0xFFFF, chkCntLfsr `shiftR` 16, chkAcc)

  -- 5c. Per-chip trace-frontier dump (diagnostics, non-fatal): flush every
  -- chip's cache and dump its leading trace words. The USIG probes (a
  -- \$display inside each picorv_unit's signature-owning process) land on
  -- whichever chip the placer put that unit's signature on, so a frozen-zero
  -- USIG means the unit's own grid-spread dataflow is dead, while a golden
  -- USIG with a zero reporter SIG isolates the loss to the
  -- signature->reporter seam routes. Non-hosting chips read all-zero.
  frontier <- try @SomeException $ forM_ runs $ \(node, gdb, bins, _) -> do
    poke64 gdb aSched flushCmd
    poke64 gdb aGmem (fromIntegral (binBase (last bins)))
    poke64 gdb aStart 1
    waitDone gdb
    ws <- forM [0 .. 15 :: Int] $ \k ->
      fromIntegral <$> (Gdb.readLe gdb (aGmemRegion + fromIntegral (k * 4)) :: IO Word32) :: IO Int
    putStrLn $ "  frontier node " <> show node <> " trace[0..15] = " <> show ws
    -- Per-link RX liveness: a window of each receive ring buffer (see the
    -- enable step before the init phases for how to read the three states).
    rxLive <- forM [0 .. 6 :: Int] $ \k -> do
      lws <- forM [0 .. 15 :: Int] $ \j ->
        Gdb.readLe gdb (rxRingAddr k "data" + fromIntegral (j * 8)) :: IO Word64
      pure (k, length (L.nub lws), L.head lws)
    forM_ rxLive $ \(k, uniq, w0) ->
      putStrLn $
        "  rxbuf node "
          <> show node
          <> " link "
          <> show k
          <> ": distinct="
          <> show uniq
          <> " sample=0x"
          <> showHex w0 ""
  case frontier of
    Left err -> putStrLn $ "  (frontier dump failed: " <> show err <> ")"
    Right () -> pure ()

  -- 6. Report + golden. Also report each chip's terminal eid.
  terminals <- forM runs $ \(node, gdb, _, exc) -> do
    e <- peek32 gdb aEid
    pure (node, e, classifyWith exc e)

  -- Leave the MUs running (monitoring) so the archived UART logs record the
  -- post-run elastic-buffer state as well.
  resumeAll
  let
    isFinish = classifyWith rExc finalEid == "FINISH"
    sigOk = sig2 == goldenFinalSig2
    chkOk = chkFinal == goldenFinalChk
    structOk = isFinish && sigOk && chkOk
  putStrLn $
    "=== Manticore MULTI RESULT (reporter node "
      <> show rNode
      <> "): eid="
      <> show finalEid
      <> " ("
      <> classifyWith rExc finalEid
      <> "), "
      <> show vcFinal
      <> " vcycles ==="
  putStrLn $
    "  golden: FINISH at ~"
      <> show goldenMultiVcycles
      <> " vcycles (+ stall-wave tail); got "
      <> show vcFinal
  putStrLn $
    "  reporter final SIG: ("
      <> show sig0
      <> ", "
      <> show sig1
      <> ", "
      <> show sig2
      <> ")  [sig2 seam-crossed golden "
      <> show goldenFinalSig2
      <> (if sigOk then " — MATCH" else " — MISMATCH")
      <> "; sig0/sig1 cross-seam too, reported only]"
  putStrLn $
    "  reporter final CHK (reporter-local multi-state $display): "
      <> show chkFinal
      <> "  [golden "
      <> show goldenFinalChk
      <> (if chkOk then " — MATCH: $display/trace mechanism verified]" else " — MISMATCH]")
  putStrLn "  per-chip terminal eids:"
  forM_ terminals $ \(node, e, k) -> putStrLn $ "    node " <> show node <> ": eid=" <> show e <> " -> " <> k
  if structOk
    then
      putStrLn
        "PASS: reporter reached FINISH via the coordinated stall wave, the multi-state $display (CHK) matches golden, AND final seam-crossed sig2 matches golden"
        >> pure ExitSuccess
    else
      putStrLn
        ( "FAIL: "
            <> ( if not isFinish
                   then "reporter did not reach FINISH"
                   else
                     if not chkOk
                       then
                         "reporter-local CHK "
                           <> show chkFinal
                           <> " != golden "
                           <> show goldenFinalChk
                           <> " — $display/trace mechanism broke"
                       else "final seam-crossed sig2=" <> show sig2 <> " != golden " <> show goldenFinalSig2
               )
        )
        >> pure (ExitFailure 1)
