-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}
-- TODO: Remove use of partial functions
{-# OPTIONS_GHC -Wno-x-partial #-}

module Bittide.Instances.Hitl.SwitchDemo.Driver where

import Clash.Prelude

import Bittide.ClockControl.Config (CcConf, defCcConf, saveCcConfig)
import Bittide.ClockControl.Topology (Topology)
import Bittide.Hitl
import Bittide.Instances.Domains (GthTx)
import Bittide.Instances.Hitl.Setup (FpgaCount, LinkCount, fpgaSetup)
import Bittide.Instances.Hitl.Utils.Driver
import Bittide.Instances.Hitl.Utils.Program
import Bittide.Wishbone (TimeCmd (Capture))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently_, mapConcurrently_)
import Control.Concurrent.Async.Extra (zipWithConcurrently, zipWithConcurrently3_)
import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class
import Data.Bifunctor (Bifunctor (bimap))
import Data.Maybe (fromJust, mapMaybe)
import Data.String.Interpolate (i)
import GHC.Stack (HasCallStack)
import Gdb (Gdb)
import Numeric (showHex)
import Project.FilePath
import Project.Handle
import Protocols.MemoryMap (
  DeviceDefinition (..),
  MemoryMap (..),
  Name (..),
  NamedLoc (..),
  PathComp (..),
  Register (..),
  convert,
  normalizeRelTree,
 )
import Protocols.MemoryMap.Check (
  MemoryMapTreeAbsNorm,
  MemoryMapTreeAnn (..),
  makeAbsolute,
 )
import System.Exit
import System.FilePath
import System.IO
import System.Process (StdStream (CreatePipe, UseHandle))
import Text.Show.Pretty (ppShow)
import Vivado.Tcl (HwTarget)
import Vivado.VivadoM
import "bittide-extra" Control.Exception.Extra (brackets)

import qualified Bittide.Calculator as Calc
import qualified Bittide.Instances.Hitl.SwitchDemo.MemoryMaps as MemoryMaps
import qualified Bittide.Instances.Hitl.Utils.OpenOcd as Ocd
import qualified Bittide.Instances.Hitl.Utils.Picocom as Picocom
import qualified Clash.Sized.Vector as V
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Gdb
import qualified System.Timeout.Extra as T

data TestStatus = TestRunning | TestDone Bool | TestTimeout deriving (Eq)

type StartDelay = 10 -- seconds

type Padding = Calc.WindowCycles FpgaCount 3
type GppeConfig = Calc.DefaultGppeConfig FpgaCount Padding
type CyclesPerWrite = Calc.CalCyclesPerWrite GppeConfig
type GroupCycles = Calc.CalGroupCycles GppeConfig
type WindowCycles = Calc.CalWindowCycles GppeConfig
type ActiveCycles = Calc.CalActiveCycles GppeConfig
type MetacycleLength = Calc.CalMetacycleLength GppeConfig

gppeConfig :: GppeConfig
gppeConfig = Calc.defaultGppeCalcConfig

{- | Convert an integer to a zero-padded 32-bit hexadecimal string. The result
is prepended by \"0x\".

>>> showHex32 0xAB
"0x000000ab"
-}
showHex32 :: (Integral a) => a -> String
showHex32 a = "0x" <> padding <> hexStr
 where
  hexStr = showHex a ""
  padding = L.replicate (8 - L.length hexStr) '0'

{- | Finds the address for a given path.

A "path" is a list of 'String's, where each item in the list is an interconnect
number, device name, or register name. Paths may end in a device name or register
name, but should not end with an interconnect number. For example, @["0", \"SwitchDemoPE\",
"buffer"]@ is a path that ends in a register name, and @["0", \"CaptureUgn0\"]@ is
a path that ends in a device name.

Caveat(s):

Device names are "canonicalized", where this means that if there are multiple
devices with the same name connected to the same interconnect, then each of them
is suffixed with their index. For instance, if your memory map looks like:

> interconnect
>   - DeviceA @ 0b000
>   - DeviceB @ 0b001
>   - DeviceC @ 0b010
>   - DeviceA @ 0b011
>   - DeviceA @ 0b100

Then after canonicalization, it will look like

> interconnect
>   - DeviceA0 @ 0b000
>   - DeviceB  @ 0b001
>   - DeviceC  @ 0b010
>   - DeviceA1 @ 0b011
>   - DeviceA2 @ 0b100

This is so that any path the function attempts to follow can only refer to one
location in memory.
-}
getPathAddress ::
  (HasCallStack, Num a) =>
  -- | Memory map to find the path in
  MemoryMap ->
  -- | The path to search for in the memory map
  [String] ->
  a
getPathAddress mm = traverseTree annAbsTree1
 where
  annRelTree = convert mm.tree
  annRelNormTree = normalizeRelTree annRelTree
  (annAbsTree0, _) = makeAbsolute mm.deviceDefs (0x0000_0000, 0xFFFF_FFFF) annRelNormTree
  annAbsTree1 = canonicalizeDevNames annAbsTree0

  canonicalizeDevNames :: (HasCallStack) => MemoryMapTreeAbsNorm -> MemoryMapTreeAbsNorm
  canonicalizeDevNames self@(AnnDeviceInstance _ _ _) = self
  canonicalizeDevNames (AnnInterconnect ann srcLoc relsAndMMs) =
    AnnInterconnect ann srcLoc
      $ doCanonicalization (M.fromList $ L.zip deviceNames (L.repeat 0)) relsAndMMs
   where
    components :: [MemoryMapTreeAbsNorm]
    components = snd <$> relsAndMMs
    deviceNames :: [String]
    deviceNames = mapMaybe getDeviceName components
    deviceNamesMult :: M.Map String Bool
    deviceNamesMult = M.fromListWith (\_ _ -> True) $ L.zip deviceNames (L.repeat False)

    getDeviceName :: (HasCallStack) => MemoryMapTreeAbsNorm -> Maybe String
    getDeviceName (AnnDeviceInstance _ _ name) = Just name
    getDeviceName _ = Nothing

    doCanonicalization ::
      (HasCallStack) =>
      M.Map String Integer ->
      [(Integer, MemoryMapTreeAbsNorm)] ->
      [(Integer, MemoryMapTreeAbsNorm)]
    doCanonicalization _ [] = []
    doCanonicalization counts ((addr, AnnInterconnect ann1 srcLoc1 subtree) : t) =
      (addr, AnnInterconnect ann1 srcLoc1 (bimap id canonicalizeDevNames <$> subtree))
        : doCanonicalization counts t
    doCanonicalization counts0 ((addr, AnnDeviceInstance ann1 srcLoc1 devName0) : t) =
      (addr, AnnDeviceInstance ann1 srcLoc1 devName1) : doCanonicalization counts1 t
     where
      needsSuffix = deviceNamesMult M.! devName0
      curCount = counts0 M.! devName0
      devName1 = if needsSuffix then devName0 <> show curCount else devName0
      counts1 =
        if needsSuffix
          then M.adjust succ devName0 counts0
          else counts0

  showPathComponent :: PathComp -> String
  showPathComponent (PathName _ s) = s
  showPathComponent (PathUnnamed n) = show n

  getTreeName :: (HasCallStack) => MemoryMapTreeAbsNorm -> String
  getTreeName (AnnInterconnect (_, showPathComponent . L.last -> name, _) _ _) = name
  getTreeName (AnnDeviceInstance _ _ name) = name

  traverseTree :: (HasCallStack, Num a) => MemoryMapTreeAbsNorm -> [String] -> a
  traverseTree (AnnInterconnect _ _ _) [] = error "Empty path given!"
  traverseTree (AnnInterconnect (_, showPathComponent . L.last -> name0, addr) _ _) [name1] =
    if name0 /= name1
      then error [i|Mismatch on interconnect name! Expected #{name0}, found #{name1}.|]
      else fromIntegral addr
  traverseTree
    (AnnInterconnect (_, showPathComponent . L.last -> name0, _) _ (fmap snd -> components))
    (name1 : next : t) =
      if name0 == name1
        then case L.find (\tree -> next == getTreeName tree) components of
          Just comp -> traverseTree comp t
          Nothing -> error [i|Failed to find device #{next} in interconnect.|]
        else error [i|Mismatch on interconnect name! Expected #{name0}, found #{name1}.|]
  traverseTree (AnnDeviceInstance (_, _, address) _ _) [] = fromIntegral address
  traverseTree (AnnDeviceInstance (_, _, _) _ name) (a : b : c) =
    error
      [i|Cannot index into #{name} farther than #{a}, but path continues: #{ppShow $ b : c}|]
  traverseTree (AnnDeviceInstance (_, _, addr) _ devName) [regName] =
    case L.find (\regNL -> regName == regNL.name.name) devDef.registers of
      Just regNL -> fromIntegral (addr + regNL.value.address)
      Nothing -> error [i|Failed to find register #{regName} in device #{devName}|]
   where
    devDef :: DeviceDefinition
    devDef = mm.deviceDefs M.! devName

muSwitchDemoPeBuffer :: (HasCallStack, Num a, Show a) => a
muSwitchDemoPeBuffer = getPathAddress MemoryMaps.mu ["0", "SwitchDemoPE", "buffer"]

dumpCcSamples :: (HasCallStack) => FilePath -> CcConf Topology -> [Gdb] -> IO ()
dumpCcSamples hitlDir ccConf ccGdbs = do
  mapConcurrently_ Gdb.interrupt ccGdbs
  nSamples <- liftIO $ zipWithConcurrently go ccGdbs ccSamplesPaths
  putStrLn [i|Dumped /n/ clock control samples: #{nSamples}|]
  saveCcConfig hitlDir ccConf
  putStrLn [i|Wrote configs and samples to: #{hitlDir}|]
 where
  go :: (HasCallStack) => Gdb -> FilePath -> IO Word
  go gdb dumpPath = do
    nSamplesWritten <- Gdb.readLe @(Unsigned 32) gdb sampleMemoryBase

    let
      bytesPerSample = 13
      bytesPerWord = 4

      dumpStart = sampleMemoryBase + bytesPerWord
      dumpEnd = dumpStart + fromIntegral nSamplesWritten * bytesPerWord * bytesPerSample

    Gdb.dumpMemoryRegion gdb dumpPath dumpStart dumpEnd >> pure (numConvert nSamplesWritten)

  sampleMemoryBase = getPathAddress @Integer MemoryMaps.cc ["0", "SampleMemory", "data"]
  ccSamplesPaths = [[i|#{hitlDir}/cc-samples-#{n}.bin|] | n <- [(0 :: Int) .. 7]]

initGdb ::
  FilePath ->
  String ->
  Gdb ->
  Ocd.TapInfo ->
  (HwTarget, DeviceInfo) ->
  IO ()
initGdb hitlDir binName gdb tapInfo (hwT, _d) = do
  Gdb.setLogging gdb
    $ hitlDir
    </> "gdb-" <> binName <> "-" <> show (getTargetIndex hwT) <> ".log"
  Gdb.arpExamine gdb ("riscv.tap" <> show tapInfo.tapId)
  Gdb.setFile gdb $ firmwareBinariesDir "riscv32imc" Release </> binName
  Gdb.setTarget gdb tapInfo.gdbPort
  Gdb.setTimeout gdb Nothing
  Gdb.runCommand gdb "echo connected to target device"
  pure ()

initPicocom :: FilePath -> (HwTarget, DeviceInfo) -> Int -> IO (ProcessHandles, IO ())
initPicocom hitlDir (_hwTarget, deviceInfo) targetIndex = do
  devNullHandle <- openFile "/dev/null" WriteMode

  let
    devPath = deviceInfo.serial
    stdoutPath = hitlDir </> "picocom-" <> show targetIndex <> "-stdout.log"
    stderrPath = hitlDir </> "picocom-" <> show targetIndex <> "-stderr.log"

  -- Note that script at `devPath` already logs to `stdoutPath` and
  -- `stderrPath`. This is what we're after: debug logging. To prevent race
  -- conditions, we need to know when picocom is ready so we also shortly
  -- interested in stderr in this Haskell process.
  (pico, cleanup) <-
    Picocom.startWithLoggingAndEnv
      ( Picocom.StdStreams
          { Picocom.stdin = CreatePipe
          , Picocom.stdout = CreatePipe
          , Picocom.stderr = UseHandle devNullHandle
          }
      )
      devPath
      stdoutPath
      stderrPath
      []

  hSetBuffering pico.stdoutHandle LineBuffering

  T.tryWithTimeout T.PrintActionTime "Waiting for \"Terminal ready\"" 10_000_000
    $ waitForLine pico.stdoutHandle "Terminal ready"

  pure (pico, cleanup)

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

  let
    hitlDir = projectDir </> "_build/hitl" </> testName

    muGetUgns :: (HwTarget, DeviceInfo) -> Gdb -> IO [(Unsigned 64, Unsigned 64)]
    muGetUgns (_, d) gdb = do
      let
        mmioAddrs :: [Integer]
        mmioAddrs =
          [ getPathAddress MemoryMaps.mu ["0", "CaptureUgn" <> show @Integer n]
          | n <- [0 .. natToNum @(LinkCount - 1)]
          ]

        readUgnMmio :: Integer -> IO (Unsigned 64, Unsigned 64)
        readUgnMmio addr = Gdb.readLe gdb addr

      liftIO $ putStrLn $ "Getting UGNs for device " <> d.deviceId
      mapM readUgnMmio mmioAddrs

    muReadPeBuffer :: (HasCallStack) => (HwTarget, DeviceInfo) -> Gdb -> IO ()
    muReadPeBuffer (_, d) gdb = do
      putStrLn $ "Reading PE buffer from device " <> d.deviceId
      let
        bufferSize :: Integer
        bufferSize = (snatToNum (SNat @FpgaCount)) * 3
      output <-
        Gdb.readCommandRaw gdb [i|x/#{bufferSize}xg #{showHex32 @Integer muSwitchDemoPeBuffer}|]
      putStrLn [i|PE buffer readout: #{output}|]

    muGetCurrentTime :: (HasCallStack) => (HwTarget, DeviceInfo) -> Gdb -> IO (Unsigned 64)
    muGetCurrentTime (_, d) gdb = do
      putStrLn $ "Getting current time from device " <> d.deviceId
      Gdb.writeLe gdb (getPathAddress @Integer MemoryMaps.mu ["0", "Timer", "command"]) Capture
      Gdb.readLe gdb (getPathAddress @Integer MemoryMaps.mu ["0", "Timer", "scratchpad"])

    muWriteCfg ::
      (HasCallStack) =>
      (HwTarget, DeviceInfo) ->
      Gdb ->
      Calc.CyclePeConfig (Unsigned 64) (Index 9) ->
      VivadoM ()
    muWriteCfg target@(_, d) gdb cfg = do
      let
        sdpePrefixed :: String -> [String]
        sdpePrefixed reg = ["0", "SwitchDemoPE", reg]

      liftIO $ do
        muReadPeBuffer target gdb
        putStrLn $ "Writing config to device " <> d.deviceId
        Gdb.writeLe @(Unsigned 64)
          gdb
          (getPathAddress MemoryMaps.mu (sdpePrefixed "read_start"))
          cfg.startReadAt
        Gdb.writeLe @(Unsigned 64)
          gdb
          (getPathAddress MemoryMaps.mu (sdpePrefixed "read_cycles"))
          (numConvert cfg.readForN)
        Gdb.writeLe @(Unsigned 64)
          gdb
          (getPathAddress MemoryMaps.mu (sdpePrefixed "write_start"))
          cfg.startWriteAt
        Gdb.writeLe @(Unsigned 64)
          gdb
          (getPathAddress MemoryMaps.mu (sdpePrefixed "write_cycles"))
          (numConvert cfg.writeForN)

    finalCheck ::
      (HasCallStack) =>
      [Gdb] ->
      [Calc.CyclePeConfig (Unsigned 64) (Index 9)] ->
      VivadoM ExitCode
    finalCheck [] _ = fail "Should pass in two or more GDBs for the final check!"
    finalCheck (_ : []) _ = fail "Should pass in two or more GDBs for the final check!"
    finalCheck gdbs@(headGdb : _) configs = do
      let
        perDeviceCheck :: Gdb -> Int -> IO Bool
        perDeviceCheck myGdb num = do
          let
            headBaseAddr = muSwitchDemoPeBuffer
            myBaseAddr = toInteger (headBaseAddr + 24 * (L.length gdbs - num - 1))
            dnaBaseAddr = getPathAddress @Integer MemoryMaps.mu ["0", "Dna", "maybe_dna"]
          myCounter <- Gdb.readLe @(Unsigned 64) headGdb myBaseAddr
          myDeviceDna <- Gdb.readLe @(Maybe (BitVector 96)) myGdb dnaBaseAddr
          headDeviceDna <- Gdb.readLe @(BitVector 96) headGdb (myBaseAddr + 0x08)

          let
            myCfg = configs L.!! num
            expectedCounter = myCfg.startWriteAt
            counterEq = myCounter == expectedCounter
            dnaEq = myDeviceDna == Just headDeviceDna
          when (not counterEq)
            $ putStrLn
              [i|Counter #{num} did not match. Found #{myCounter}, expected #{expectedCounter}|]
          when (not dnaEq)
            $ putStrLn
              [i|DNA did not match. Found #{headDeviceDna}, expected #{myDeviceDna}|]
          return $ counterEq && dnaEq

      deviceChecks <- forM (L.zip gdbs [0 ..]) $ \(gdb, n) -> liftIO $ perDeviceCheck gdb n
      let result = if and deviceChecks then ExitSuccess else ExitFailure 2
      return result

  forM_ targets (assertProbe "probe_test_start")

  let
    -- Expected JTAG IDs for MU and CC TAPs in that order
    expectedJtagIds = [0x0_514C001, 0x1_514C001, 0x2_514C001]
    openOcdStarts = liftIO <$> L.zipWith (Ocd.initOpenOcd expectedJtagIds hitlDir) targets [0 ..]
  brackets openOcdStarts (liftIO . (.cleanup)) $ \initOcdsData -> do
    let
      allTapInfos = (.tapInfos) <$> initOcdsData

      bootTapInfos, muTapInfos, ccTapInfos :: [Ocd.TapInfo]
      (bootTapInfos, muTapInfos, ccTapInfos)
        | all (== L.length expectedJtagIds) (L.length <$> allTapInfos)
        , [boots, mus, ccs] <- L.transpose allTapInfos =
            (boots, mus, ccs)
        | otherwise =
            error
              $ "Unexpected number of OpenOCD taps initialized. Expected: "
              <> show (L.length expectedJtagIds)
              <> ", but got: "
              <> show (L.length <$> allTapInfos)

      bootPorts = (.gdbPort) <$> bootTapInfos
      muPorts = (.gdbPort) <$> muTapInfos
      ccPorts = (.gdbPort) <$> ccTapInfos

    Gdb.withGdbs (L.length targets) $ \bootGdbs -> do
      liftIO $ zipWithConcurrently3_ (initGdb hitlDir "boot") bootGdbs bootTapInfos targets
      liftIO $ mapConcurrently_ ((errorToException =<<) . Gdb.loadBinary) bootGdbs

      let picocomStarts = liftIO <$> L.zipWith (initPicocom hitlDir) targets [0 ..]
      brackets picocomStarts (liftIO . snd) $ \(L.map fst -> picocoms) -> do
        liftIO $ mapConcurrently_ Gdb.continue bootGdbs
        liftIO
          $ T.tryWithTimeout T.PrintActionTime "Waiting for done" 60_000_000
          $ forConcurrently_ picocoms
          $ \pico ->
            waitForLine pico.stdoutHandle "[BT] Done"

        pure ExitSuccess

-- Gdb.withGdbs (L.length targets) $ \ccGdbs -> do
--   liftIO $ zipWithConcurrently3_ (initGdb hitlDir "clock-control") ccGdbs ccPorts targets
--   liftIO $ mapConcurrently_ ((errorToException =<<) . Gdb.loadBinary) ccGdbs

--   Gdb.withGdbs (L.length targets) $ \muGdbs -> do
--     liftIO $ zipWithConcurrently3_ (initGdb hitlDir "switch-demo1-mu") muGdbs muPorts targets
--     liftIO $ mapConcurrently_ ((errorToException =<<) . Gdb.loadBinary) muGdbs

--     let goDumpCcSamples = dumpCcSamples hitlDir (defCcConf (natToNum @FpgaCount)) ccGdbs
--     liftIO $ mapConcurrently_ Gdb.continue ccGdbs
--     liftIO
--       $ T.tryWithTimeoutOn T.PrintActionTime "Waiting for stable links" 60_000_000 goDumpCcSamples
--       $ forConcurrently_ picocoms
--       $ \pico ->
--         waitForLine pico.stdoutHandle "[CC] All links stable"

--     liftIO $ mapConcurrently_ Gdb.continue muGdbs
--     liftIO
--       $ T.tryWithTimeoutOn
--         T.PrintActionTime
--         "Wait for elastic buffers to be centered"
--         60_000_000
--         goDumpCcSamples
--       $ forConcurrently_ picocoms
--       $ \pico ->
--         waitForLine pico.stdoutHandle "[MU] All elastic buffers centered"

--     liftIO
--       $ T.tryWithTimeoutOn
--         T.PrintActionTime
--         "Waiting for captured UGNs"
--         (3 * 60_000_000)
--         goDumpCcSamples
--       $ forConcurrently_ picocoms
--       $ \pico ->
--         waitForLine pico.stdoutHandle "[MU] All UGNs captured"

--     liftIO $ putStrLn "Getting UGNs for all targets"
--     liftIO $ mapConcurrently_ Gdb.interrupt muGdbs
--     ugnPairsTable <- liftIO $ zipWithConcurrently muGetUgns targets muGdbs
--     let
--       ugnPairsTableV = fromJust . V.fromList $ fromJust . V.fromList <$> ugnPairsTable
--     liftIO $ do
--       putStrLn "Calculating IGNs for all targets"
--       Calc.printAllIgns ugnPairsTableV fpgaSetup
--       mapM_ print ugnPairsTableV
--     currentTime <- liftIO $ muGetCurrentTime (L.head targets) (L.head muGdbs)
--     let
--       startOffset = currentTime + natToNum @(PeriodToCycles GthTx (Seconds StartDelay))
--       metaChainConfig ::
--         Vec FpgaCount (Calc.DefaultGppeMetaPeConfig (Unsigned 64) FpgaCount 3 Padding)
--       metaChainConfig =
--         Calc.fullChainConfiguration gppeConfig fpgaSetup ugnPairsTableV startOffset
--       chainConfig :: Vec FpgaCount (Calc.CyclePeConfig (Unsigned 64) (Index (FpgaCount + 1)))
--       chainConfig =
--         Calc.metaPeConfigToCyclePeConfig (natToNum @MetacycleLength)
--           <$> metaChainConfig
--     liftIO $ do
--       putStrLn [i|Starting clock cycle: #{startOffset}|]
--       putStrLn [i|Cycles per write: #{natToNum @CyclesPerWrite :: Integer}|]
--       putStrLn [i|Cycles per group: #{natToNum @GroupCycles :: Integer}|]
--       putStrLn [i|Cycles per window: #{natToNum @WindowCycles :: Integer}|]
--       putStrLn [i|Cycles per active period: #{natToNum @ActiveCycles :: Integer}|]
--       putStrLn [i|Cycles of padding: #{natToNum @Padding :: Integer}|]
--       putStrLn [i|Cycles per metacycle: #{natToNum @MetacycleLength :: Integer}|]
--       putStrLn "Calculated the following configs for the switch processing elements:"
--       forM_ metaChainConfig print
--       forM_ chainConfig print
--     _ <- sequenceA $ L.zipWith3 muWriteCfg targets muGdbs (toList chainConfig)
--     liftIO $ do
--       let delayMicros = natToNum @StartDelay * 1_250_000
--       threadDelay delayMicros
--       putStrLn [i|Slept for: #{delayMicros}μs|]
--       newCurrentTime <- muGetCurrentTime (L.head targets) (L.head muGdbs)
--       putStrLn [i|Clock is now: #{newCurrentTime}|]

--     _ <- liftIO $ sequenceA $ L.zipWith muReadPeBuffer targets muGdbs

--     bufferExit <- finalCheck muGdbs (toList chainConfig)

--     liftIO goDumpCcSamples

--     pure bufferExit
