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
over the management-unit (MU) GDB instead of JTAG-to-AXI, and through the
chip's DMI gmem window instead of a flat AXI memory:

  1. Bring up the boot CPU (Si539x clock config) — reuse the demo boot
     firmware; it is independent of the user core.
  2. Attach GDB to the (halted) MU; its bus reaches the chip's Wishbone host
     registers (ManticoreControl / ManticoreDmi), so the driver pokes them
     directly — no Manticore-specific MU firmware needed.
  3. Load the program image (manifest.json + exec.bin streams produced by
     .github/scripts/manticore_compile_program.sh) into gmem over the DMI
     window; zero words are skipped (BRAM powers up to zero).
  4. Run each initializer (CMD_START, expect FINISH), then the main program;
     on each FLUSH (a @$display@) read the trace record over the DMI window and
     resume, until FINISH.
  5. Check the collected trace against the golden values.

NB milestone 1: single chip, MU halted + poked over GDB. The MU/clock-control
firmware is not run (a single chip needs no link startup / clock grooming).
-}
module Bittide.Instances.Hitl.ManticoreDemo.Driver where

import Prelude

import Bittide.Hitl (DeviceInfo)
import Bittide.Instances.Hitl.Utils.Driver (assertProbe)
import Bittide.Instances.Hitl.Utils.Gdb (initGdb)
import Bittide.Instances.Hitl.Utils.MemoryMap (getPathAddress)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.Async.Extra (zipWithConcurrently3_)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (Array, Number, Object, String))
import Data.Bits (shiftL, (.|.))
import Data.Default (def)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Word (Word16, Word32, Word64)
import Project.FilePath (findParentContaining)
import Project.Handle (assertEither)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Vivado.Tcl (HwTarget)
import Vivado.VivadoM (VivadoM)
import "bittide-extra" Control.Exception.Extra (brackets)

import qualified Bittide.Instances.Hitl.ManticoreDemo.MemoryMaps as MemoryMaps
import qualified Bittide.Instances.Hitl.Utils.OpenOcd as Ocd
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Text as Text
import qualified Gdb

-- ---------------------------------------------------------------------------
-- Manifest (subset of the manticore-runtime manifest.json)
-- ---------------------------------------------------------------------------

data Manifest = Manifest
  { userBase :: Int
  -- ^ reserved global-memory words (the @base@ field, default 0x4000)
  , initializers :: [FilePath]
  , program :: FilePath
  , exceptions :: [(Int, String)]
  -- ^ (exception id, type), e.g. (1, "FINISH")
  }

parseManifest :: FilePath -> IO Manifest
parseManifest path = do
  bytes <- Aeson.eitherDecodeFileStrict path
  obj <- case bytes of
    Right (Object o) -> pure o
    Right _ -> fail (path <> ": not a JSON object")
    Left e -> fail (path <> ": " <> e)
  let
    asString (String s) = Just (Text.unpack s)
    asString _ = Nothing
    asInt (Number n) = Just (round n)
    asInt _ = Nothing
    items k = case KM.lookup k obj of Just (Array v) -> foldr (:) [] v; _ -> []
  program <- case KM.lookup "program" obj >>= asString of
    Just p -> pure p
    Nothing -> fail (path <> ": missing 'program'")
  let
    initializers = mapMaybe asString (items "initializers")
    userBase = fromMaybe 0x4000 (KM.lookup "base" obj >>= asInt)
    exceptions =
      [ (eid, ty)
      | Object e <- items "exceptions"
      , Just eid <- [KM.lookup "eid" e >>= asInt]
      , Just ty <- [KM.lookup "type" e >>= asString]
      ]
  pure Manifest{userBase, initializers, program, exceptions}

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

-- | Lay out the initializers then the main program above the reserved region.
layout :: FilePath -> Manifest -> IO [Binary]
layout mdir m = do
  (initBins, next) <- go ([], userBase m) (zip [(0 :: Int) ..] (initializers m))
  mw <- readBinWords (resolve (program m))
  pure (initBins <> [Binary "main" next mw False])
 where
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

mainTimeout, initTimeout :: Word64
mainTimeout = 50_000_000
initTimeout = 1_000_000

goldenTrace :: [Int]
goldenTrace = [0, 0, 1, 3, 6, 10, 15, 21, 28, 36, 45]

-- ---------------------------------------------------------------------------
-- Driver
-- ---------------------------------------------------------------------------

regAddr :: String -> String -> Integer
regAddr dev reg =
  either (error . (("manticore reg " <> dev <> "." <> reg <> ": ") <>)) id $
    getPathAddress MemoryMaps.managementUnit ["0", dev, reg]

driver ::
  String ->
  [(HwTarget, DeviceInfo)] ->
  VivadoM ExitCode
driver testName targets = do
  liftIO . putStrLn $ "Manticore demo driver: " <> show (length targets) <> " target(s)"
  forM_ targets (assertProbe "probe_test_start")

  projectDir <- liftIO $ findParentContaining "cabal.project"
  let
    hitlDir = projectDir </> "_build/hitl" </> testName
    programDir = projectDir </> "_build/manticore/program"
    expectedJtagIds = [0x0514C001, 0x1514C001, 0x2514C001] -- boot / MU / CC
    toInitArgs (_, deviceInfo) targetIndex =
      Ocd.InitOpenOcdArgs{deviceInfo, expectedJtagIds, hitlDir, targetIndex}
    initArgs = L.zipWith toInitArgs targets [0 ..]

  manifest <- liftIO $ parseManifest (programDir </> "manifest.json")
  bins <- liftIO $ layout programDir manifest

  let
    bootInitArgs = L.repeat def{Ocd.logPrefix = "boot-", Ocd.initTcl = "vexriscv_boot_init.tcl"}
    openOcdBootStarts = liftIO <$> L.zipWith Ocd.initOpenOcd initArgs bootInitArgs

  -- Boot CPU: configure the Si539x clocks (the demo boot firmware), then idle.
  brackets openOcdBootStarts (liftIO . (.cleanup)) $ \initOcdsData -> do
    let bootTapInfos = Ocd.parseBootTapInfo <$> initOcdsData
    Gdb.withGdbs (L.length targets) $ \bootGdbs -> do
      liftIO $ zipWithConcurrently3_ (initGdb hitlDir "manticore-demo-boot") bootGdbs bootTapInfos targets
      liftIO $ mapConcurrently_ ((assertEither =<<) . Gdb.loadBinary) bootGdbs
      liftIO $ mapConcurrently_ Gdb.continue bootGdbs
      liftIO $ threadDelay 3_000_000

  -- Clock-control + management-unit CPUs: run the generic bring-up firmware
  -- (clock control + link startup) on every node to bring the Bittide domain
  -- up — without it the chip's Wishbone host registers do not respond. The MU
  -- firmware idles after bring-up; halt it, then poke the chip over GDB.
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
        -- Run the bring-up: clock control (keeps running) + link startup.
        liftIO $ mapConcurrently_ Gdb.continue ccGdbs
        liftIO $ mapConcurrently_ Gdb.continue muGdbs
        -- Give the MU firmware time to finish link startup + stability.
        liftIO $ threadDelay 20_000_000
        -- Halt the (now idling) MUs so we can poke the chip registers.
        liftIO $ mapConcurrently_ Gdb.interrupt muGdbs
        case zip muGdbs targets of
          [] -> pure ExitSuccess
          ((gdb, _) : _) -> liftIO $ runManticore gdb bins (exceptions manifest)
 where
  pick i xss = [xs !! i | xs <- xss, length xs > i]

runManticore :: Gdb.Gdb -> [Binary] -> [(Int, String)] -> IO ExitCode
runManticore gdb bins excMap = do
  let
    aSched = regAddr "ManticoreControl" "schedule_config"
    aGmem = regAddr "ManticoreControl" "gmem_base"
    aTrace = regAddr "ManticoreControl" "trace_base"
    aStart = regAddr "ManticoreControl" "start"
    aEid = regAddr "ManticoreControl" "exception_id"
    aVc = regAddr "ManticoreControl" "virtual_cycles"
    aDone = regAddr "ManticoreControl" "done"
    aIdle = regAddr "ManticoreControl" "idle"
    aDmiAddr = regAddr "ManticoreDmi" "dmi_addr"
    aDmiWdata = regAddr "ManticoreDmi" "dmi_wdata"
    aDmiRdata = regAddr "ManticoreDmi" "dmi_rdata"

    poke64 :: Integer -> Word64 -> IO ()
    poke64 a v = Gdb.writeLe gdb a v
    poke16 :: Integer -> Word16 -> IO ()
    poke16 a v = Gdb.writeLe gdb a v
    peek32 :: Integer -> IO Int
    peek32 a = fromIntegral <$> (Gdb.readLe gdb a :: IO Word32)
    peek64 :: Integer -> IO Int
    peek64 a = fromIntegral <$> (Gdb.readLe gdb a :: IO Word64)

    dmiWrite :: Int -> Word16 -> IO ()
    dmiWrite off val = poke64 aDmiAddr (fromIntegral off) >> poke16 aDmiWdata val
    dmiRead :: Int -> IO Int
    dmiRead off = do
      poke64 aDmiAddr (fromIntegral off)
      _ <- (Gdb.readLe gdb aDmiRdata :: IO Word16) -- registered read: 1-cycle latency
      fromIntegral <$> (Gdb.readLe gdb aDmiRdata :: IO Word16)

    classify eid
      | eid > (0xFFFF :: Int) = "TIMEOUT"
      | otherwise = fromMaybe "unknown" (lookup eid excMap)

    waitDone = go (0 :: Int)
     where
      go n
        | n > 200000 = fail "Manticore: timeout waiting for done/idle"
        | otherwise = do
            d <- peek32 aDone
            i <- peek32 aIdle
            when (d == 0 && i == 0) (go (n + 1))

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

  putStrLn "Loading Manticore image over the DMI window (skipping zero words)..."
  forM_ bins $ \b ->
    forM_ (zip [binBase b ..] (binWords b)) $ \(off, w) ->
      when (w /= 0) (dmiWrite off w)

  poke64 aTrace 0

  forM_ (filter binIsInit bins) $ \b -> runCmd (binName b) (binBase b) (startCmd initTimeout)
  (_, k0) <- runCmd "main" (binBase (last bins)) (startCmd mainTimeout)

  traceRef <- newIORef []
  let
    -- One $display record: value is gmem words 2..3 (offsets 4 and 6 bytes →
    -- word indices 2,3), per run_manifest.py's RF-write decode.
    -- value low/high half-words: gmem 16-bit words 5 and 6 (run_manifest.py's
    -- 32-bit words 2..3 RF-write decode, in 16-bit-word terms).
    readTraceVal = do
      lo <- dmiRead 5
      hi <- dmiRead 6
      pure (lo .|. (hi `shiftL` 16) :: Int)
    loop kind guard
      | kind == "FLUSH" && guard < (100000 :: Int) = do
          poke64 aSched flushCmd >> poke64 aStart 1 >> waitDone
          v <- readTraceVal
          modifyIORef' traceRef (<> [v])
          poke64 aSched (resumeCmd mainTimeout) >> poke64 aStart 1 >> waitDone
          eid <- peek32 aEid
          loop (classify eid) (guard + 1)
      | otherwise = pure kind
  finalKind <- loop k0 0
  trace <- readIORef traceRef

  putStrLn $ "=== Manticore RESULT: " <> finalKind <> ", " <> show (length trace) <> " records ==="
  putStrLn $ "  trace:  " <> show trace
  putStrLn $ "  golden: " <> show goldenTrace
  if finalKind `elem` ["FINISH", "STOP"]
    then putStrLn "PASS: Manticore terminated normally" >> pure ExitSuccess
    else putStrLn ("FAIL: Manticore ended as " <> finalKind) >> pure (ExitFailure 1)
