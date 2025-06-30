-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
-- TODO: Remove use of partial functions
{-# OPTIONS_GHC -Wno-x-partial #-}

module Bittide.Instances.Hitl.Driver.SwitchDemo (
  OcdInitData (..),
  ccWhoAmID,
  driver,
  dumpCcSamples,
  gppeWhoAmID,
  initGdb,
  initOpenOcd,
  initPicocom,
  muWhoAmID,
  whoAmIPrefix,
) where

import Clash.Prelude

import Bittide.ClockControl.Config (CcConf, defCcConf, saveCcConfig)
import Bittide.ClockControl.Topology (Topology)
import Bittide.Hitl
import Bittide.Instances.Domains
import Bittide.Instances.Hitl.SwitchDemo
import Bittide.Instances.Hitl.Setup (FpgaCount, fpgaSetup)
import Bittide.Instances.Hitl.Utils.Driver
import Bittide.Instances.Hitl.Utils.Program
import Bittide.Wishbone (TimeCmd (Capture))
import Clash.Sized.Extra (extendLsb0s)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently_, mapConcurrently_)
import Control.Concurrent.Async.Extra (zipWithConcurrently, zipWithConcurrently3_)
import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class
import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (chr, isDigit)
import Data.Maybe (fromJust, fromMaybe)
import Data.String.Interpolate (i)
import GHC.Stack (HasCallStack)
import Gdb (Gdb)
import Numeric (showHex)
import Project.FilePath
import Project.Handle
import Protocols.MemoryMap (
  DeviceDefinition (..),
  DeviceDefinitions,
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
import System.Timeout.Extra (tryWithTimeout, tryWithTimeoutOn)
import Text.Show.Pretty (ppShow)
import Vivado.Tcl (HwTarget)
import Vivado.VivadoM
import "bittide-extra" Control.Exception.Extra (brackets)

import qualified Bittide.Calculator as Calc
import qualified Bittide.Instances.Hitl.Utils.OpenOcd as Ocd
import qualified Bittide.Instances.Hitl.Utils.Picocom as Picocom
import qualified Clash.Sized.Vector as V
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Gdb

data OcdInitData = OcdInitData
  { muPort :: Int
  -- ^ Management unit GDB port
  , ccPort :: Int
  -- ^ Clock control GDB port
  , gppePort :: Int
  -- ^ GPPE GDB port
  , handles :: ProcessHandles
  -- ^ OpenOCD stdio handles
  , cleanup :: IO ()
  -- ^ Cleanup function
  }

type StartDelay = 5 -- seconds

whoAmIPrefix :: forall n m. (KnownNat n, KnownNat m, n ~ m + 3) => Unsigned n
whoAmIPrefix = extendLsb0s @3 @m (0b111 :: Unsigned 3)
ccWhoAmID :: BitVector 32
ccWhoAmID = $(makeWhoAmIDTH "swcc")
muWhoAmID :: BitVector 32
muWhoAmID = $(makeWhoAmIDTH "mgmt")
gppeWhoAmID :: BitVector 32
gppeWhoAmID = $(makeWhoAmIDTH "gppe")

type Padding = Calc.WindowCycles FpgaCount 31
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

dumpCcSamples :: (HasCallStack) => FilePath -> CcConf Topology -> [Gdb] -> IO ()
dumpCcSamples hitlDir ccConf ccGdbs = do
  mapConcurrently_ Gdb.interrupt ccGdbs
  nSamples <- zipWithConcurrently go ccGdbs ccSamplesPaths
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

  sampleMemoryBase = ccSampleMemoryReg @Integer
  ccSamplesPaths = [[i|#{hitlDir}/cc-samples-#{n}.bin|] | n <- [(0 :: Int) .. 7]]

getMemoryMapTreeAbsNorm :: MemoryMap -> MemoryMapTreeAbsNorm
getMemoryMapTreeAbsNorm mm = annAbsTree
 where
  MemoryMap{..} = mm
  annRelTree = convert tree
  annRelNormTree = normalizeRelTree annRelTree
  (annAbsTree, _) = makeAbsolute deviceDefs (0x0000_0000, 0xFFFF_FFFF) annRelNormTree

{- | This is a very minimal implementation, only covering the cases currently needed
for the SwitchDemo processing elements. This fails to account for several things:

 - Interconnect depth greater than 1
 - 'AnnWithName' instances
 - 'AnnWithTag' instances
 - 'AnnAbsAddr' instances
 - 'AnnNormWrapper' instances

The output of this is a list of 'String's and a generic numeric type 'a'. The
'String's are formatted like @0/DeviceName.register_name@.
-}
enumerateRegisters ::
  (HasCallStack, Num a, Show a) =>
  DeviceDefinitions ->
  MemoryMapTreeAbsNorm ->
  [(String, a)]
enumerateRegisters dd mm = getCanonicalNames allPaths (makeCounterMap allDevices)
 where
  enumeratePaths ::
    (HasCallStack, Num a, Show a) =>
    DeviceDefinitions ->
    MemoryMapTreeAbsNorm ->
    [([String], (String, a))]
  enumeratePaths d m =
    fmap (bimap id (bimap id fromIntegral))
      $ case m of
        AnnInterconnect _ _ (fmap snd -> comps) -> L.foldl (<>) [] (enumeratePaths d <$> comps)
        AnnDeviceInstance (_, path, addr) _ devName ->
          regPaths <> [(devPath, ("", addr))]
         where
          devDef = fromJust $ M.lookup devName d
          devPath = (showPathComp <$> L.init path) <> [devName]
          regPaths =
            fmap (\nl -> (devPath, (nl.name.name, addr + nl.value.address))) devDef.registers
  allPaths :: (HasCallStack, Num a, Show a) => [([String], (String, a))]
  allPaths = enumeratePaths dd mm
  allDevices :: [[String]]
  allDevices = fmap fst $ filter (\(_, (reg, _)) -> L.null reg) (allPaths @Integer)
  makeCounterMap :: (HasCallStack) => [[String]] -> M.Map [String] Integer
  makeCounterMap pathList = M.fromList $ fmap (\path -> (path, 0)) pathList
  getPathCounts :: (HasCallStack) => [[String]] -> M.Map [String] Integer
  getPathCounts pathList = M.fromListWith (\a b -> 1 + max a b) startList
   where
    startList = fmap (\path -> (path, 0)) pathList
  allPathCounts :: M.Map [String] Integer
  allPathCounts = getPathCounts $ allDevices
  getCanonicalNames ::
    (HasCallStack, Num a, Show a) =>
    [([String], (String, a))] ->
    M.Map [String] Integer ->
    [(String, a)]
  getCanonicalNames [] _ = []
  getCanonicalNames ((devPath0, (regName, addr)) : t) curCounts0 =
    if L.null regName
      -- Device instance
      then
        let
          curCounts1 = M.update (\n -> Just (n + 1)) devPath0 curCounts0
         in
          (devPath1, addr) : getCanonicalNames t curCounts1
      -- Register instance
      else
        let
          myPath = devPath1 <> "." <> regName
         in
          (myPath, addr) : getCanonicalNames t curCounts0
   where
    maxCount = allPathCounts M.! devPath0
    devPath1 =
      if maxCount > 0
        then L.intercalate "/" devPath0 <> show (curCounts0 M.! devPath0)
        else L.intercalate "/" devPath0
  showPathComp :: PathComp -> String
  showPathComp (PathName _ s) = s
  showPathComp (PathUnnamed n) = show n

getRegisterAddress :: (HasCallStack, Num a, Show a) => [(String, a)] -> String -> a
getRegisterAddress addrs path =
  case results of
    [] -> error [i|No register found with path #{path}!|]
    [(_, addr)] -> addr
    _ -> error [i|Multiple register found with path #{path}! Paths: #{ppShow results}|]
 where
  results = filter (\(p, _) -> p == path) addrs

ccAnnAbsTree :: MemoryMapTreeAbsNorm
ccAnnAbsTree = getMemoryMapTreeAbsNorm memoryMapCc

ccRegisterPaths :: (HasCallStack, Num a, Show a) => [(String, a)]
ccRegisterPaths = enumerateRegisters memoryMapCc.deviceDefs ccAnnAbsTree

getCcRegisterAddress :: (HasCallStack, Num a, Show a) => String -> a
getCcRegisterAddress = getRegisterAddress ccRegisterPaths

ccSampleMemoryReg :: (HasCallStack, Num a, Show a) => a
ccSampleMemoryReg = getCcRegisterAddress "0/SampleMemory.data"

muAnnAbsTree :: MemoryMapTreeAbsNorm
muAnnAbsTree = getMemoryMapTreeAbsNorm memoryMapMu

muRegisterPaths :: (HasCallStack, Num a, Show a) => [(String, a)]
muRegisterPaths = enumerateRegisters memoryMapMu.deviceDefs muAnnAbsTree

getMuRegisterAddress :: (HasCallStack, Num a, Show a) => String -> a
getMuRegisterAddress = getRegisterAddress muRegisterPaths

muTimerCmdReg :: (HasCallStack, Num a, Show a) => a
muTimerCmdReg = getMuRegisterAddress "0/Timer.command"
muTimerScratchpadReg :: (HasCallStack, Num a, Show a) => a
muTimerScratchpadReg = getMuRegisterAddress "0/Timer.scratchpad"
muCaptureUgnDevs :: (HasCallStack, Num a, Show a) => [a]
muCaptureUgnDevs =
  if L.length captureUgnDevs == (natToNum @FpgaCount) - 1
    then snd <$> captureUgnDevs
    else error [i|Found only #{L.length captureUgnDevs} `CaptureUgn` devices.|]
 where
  captureUgnDevs =
    L.filter (\(p, _) -> L.isPrefixOf "0/CaptureUgn" p && isDigit (L.last p)) muRegisterPaths

gppeAnnAbsTree :: MemoryMapTreeAbsNorm
gppeAnnAbsTree = getMemoryMapTreeAbsNorm memoryMapGppe

gppeRegisterPaths :: (HasCallStack, Num a, Show a) => [(String, a)]
gppeRegisterPaths = enumerateRegisters memoryMapGppe.deviceDefs gppeAnnAbsTree

getGppeRegisterAddress :: (HasCallStack, Num a, Show a) => String -> a
getGppeRegisterAddress = getRegisterAddress gppeRegisterPaths

gppeDnaReg :: (HasCallStack, Num a, Show a) => a
gppeDnaReg = getGppeRegisterAddress "0/Dna.dna"
gppeWriteMetacycleReg :: (HasCallStack, Num a, Show a) => a
gppeWriteMetacycleReg = getGppeRegisterAddress "0/MetaPeConfig.write_metacycle"
gppeWriteOffsetReg :: (HasCallStack, Num a, Show a) => a
gppeWriteOffsetReg = getGppeRegisterAddress "0/MetaPeConfig.write_offset"
gppeWriteNAddrsReg :: (HasCallStack, Num a, Show a) => a
gppeWriteNAddrsReg = getGppeRegisterAddress "0/MetaPeConfig.write_n_addresses"
gppeReadMetacycleReg :: (HasCallStack, Num a, Show a) => a
gppeReadMetacycleReg = getGppeRegisterAddress "0/MetaPeConfig.read_metacycle"
gppeReadOffsetReg :: (HasCallStack, Num a, Show a) => a
gppeReadOffsetReg = getGppeRegisterAddress "0/MetaPeConfig.read_offset"
gppeReadNAddrsReg :: (HasCallStack, Num a, Show a) => a
gppeReadNAddrsReg = getGppeRegisterAddress "0/MetaPeConfig.read_n_addresses"
gppeBufferReg :: (HasCallStack, Num a, Show a) => a
gppeBufferReg = getGppeRegisterAddress "0/MetaPeConfig.buffer"

gppeWriteCfg ::
  (HasCallStack) =>
  (HwTarget, DeviceInfo) ->
  Gdb ->
  Calc.DefaultGppeMetaPeConfig (Unsigned 64) FpgaCount 3 Padding ->
  VivadoM ()
gppeWriteCfg (_, d) gdb cfg = do
  liftIO $ do
    putStrLn $ "Writing config " <> show cfg <> " to device " <> d.deviceId
    Gdb.writeLe gdb gppeWriteMetacycleReg cfg.writeMetacycle
    Gdb.writeLe gdb gppeWriteOffsetReg cfg.writeOffset
    Gdb.writeLe gdb gppeWriteNAddrsReg cfg.writeForN
    Gdb.writeLe gdb gppeReadMetacycleReg cfg.readMetacycle
    Gdb.writeLe gdb gppeReadOffsetReg cfg.readOffset
    Gdb.writeLe gdb gppeReadNAddrsReg cfg.readForN

initOpenOcd :: FilePath -> (HwTarget, DeviceInfo) -> Int -> IO OcdInitData
initOpenOcd hitlDir (_, d) targetIndex = do
  putStrLn $ "Starting OpenOCD for target " <> d.deviceId

  let
    gdbPortMU = 3333 + targetIndex * 3
    gdbPortCC = gdbPortMU + 1
    gdbPortGPPE = gdbPortMU + 2
    ocdStdout = hitlDir </> "openocd-" <> show targetIndex <> "-stdout.log"
    ocdStderr = hitlDir </> "openocd-" <> show targetIndex <> "-stderr.log"
    tclPort = 6666 + targetIndex
    telPort = 4444 + targetIndex
  putStrLn $ "logging OpenOCD stdout to `" <> ocdStdout <> "`"
  putStrLn $ "logging OpenOCD stderr to `" <> ocdStderr <> "`"

  putStrLn "Starting OpenOCD..."
  (ocd, ocdPh, ocdClean0) <-
    Ocd.startOpenOcdWithEnvAndArgs
      ["-f", "sipeed.tcl", "-f", "vexriscv-3chain.tcl"]
      [ ("OPENOCD_STDOUT_LOG", ocdStdout)
      , ("OPENOCD_STDERR_LOG", ocdStderr)
      , ("USB_DEVICE", d.usbAdapterLocation)
      , ("DEV_A_GDB", show gdbPortCC)
      , ("DEV_B_GDB", show gdbPortGPPE)
      , ("DEV_C_GDB", show gdbPortMU)
      , ("TCL_PORT", show tclPort)
      , ("TEL_PORT", show telPort)
      ]
  hSetBuffering ocd.stderrHandle LineBuffering
  tryWithTimeout "Waiting for OpenOCD to start" 15_000_000
    $ expectLine ocd.stderrHandle Ocd.waitForHalt

  let
    ocdProcName = "OpenOCD (" <> d.deviceId <> ")"
    ocdClean1 = ocdClean0 >> awaitProcessTermination ocdProcName ocdPh (Just 10_000_000)

  return $ OcdInitData gdbPortMU gdbPortCC gdbPortGPPE ocd ocdClean1

initGdb ::
  FilePath ->
  String ->
  Gdb ->
  Int ->
  (HwTarget, DeviceInfo) ->
  IO ()
initGdb hitlDir binName gdb gdbPort (hwT, _d) = do
  Gdb.setLogging gdb
    $ hitlDir
    </> "gdb-" <> binName <> "-" <> show (getTargetIndex hwT) <> ".log"
  Gdb.setFile gdb $ firmwareBinariesDir "riscv32imc" Release </> binName
  Gdb.setTarget gdb gdbPort
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

  tryWithTimeout "Waiting for \"Terminal ready\"" 10_000_000
    $ waitForLine pico.stdoutHandle "Terminal ready"

  pure (pico, cleanup)

gdbCheck :: BitVector 32 -> Gdb -> VivadoM ExitCode
gdbCheck expectedBE gdb = do
  let whoAmIBase = whoAmIPrefix @32
  bytes <- Gdb.readBytes @4 gdb (fromIntegral whoAmIBase)
  let
    bytesAsInts = L.map fromIntegral $ V.toList $ bytes
    expectedLE = reverse $ bitCoerce @_ @(Vec 4 (BitVector 8)) expectedBE
    expectedS =
      L.map (chr . fromIntegral)
        $ V.toList expectedLE
  liftIO $ putStrLn [i|Read whoAmID: '#{L.map chr bytesAsInts}', expected '#{expectedS}'|]
  pure $ if bytes == expectedLE then ExitSuccess else ExitFailure 1

foldExitCodes :: VivadoM (Int, ExitCode) -> ExitCode -> VivadoM (Int, ExitCode)
foldExitCodes prev code = do
  (count, acc) <- prev
  return
    $ if code == ExitSuccess
      then (count + 1, acc)
      else (count, code)

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
        readUgnMmio :: Integer -> IO (Unsigned 64, Unsigned 64)
        readUgnMmio addr = Gdb.readLe gdb addr

      liftIO $ putStrLn $ "Getting UGNs for device " <> d.deviceId
      mapM readUgnMmio muCaptureUgnDevs

    gppeReadBuffer :: (HasCallStack) => (HwTarget, DeviceInfo) -> Gdb -> IO ()
    gppeReadBuffer (_, d) gdb = do
      putStrLn $ "Reading PE buffer from device " <> d.deviceId
      let
        start = gppeBufferReg @(Unsigned 32)
        bufferSize :: Integer
        bufferSize = (snatToNum (SNat @FpgaCount)) * 3
      output <- Gdb.readCommandRaw gdb [i|x/#{bufferSize}xg #{showHex32 start}|]
      putStrLn [i|PE buffer readout: #{output}|]

    muGetCurrentTime :: (HasCallStack) => (HwTarget, DeviceInfo) -> Gdb -> IO (Unsigned 64)
    muGetCurrentTime (_, d) gdb = do
      putStrLn $ "Getting current time from device " <> d.deviceId
      Gdb.writeLe gdb muTimerCmdReg Capture
      Gdb.readLe gdb muTimerScratchpadReg

    finalCheck ::
      (HasCallStack) =>
      [Gdb] ->
      [Gdb] ->
      [Calc.DefaultGppeMetaPeConfig (Unsigned 64) FpgaCount 3 Padding] ->
      VivadoM ExitCode
    finalCheck [] _ _ = fail "Should pass in two or more GDBs for the final check!"
    finalCheck _ [] _ = fail "Should pass in two or more GDBs for the final check!"
    finalCheck [_] _ _ = fail "Should pass in two or more GDBs for the final check!"
    finalCheck _ [_] _ = fail "Should pass in two or more GDBs for the final check!"
    finalCheck muGdbs@(headMuGdb : _) (headGppeGdb : _) configs = do
      let
        perDeviceCheck :: Gdb -> Int -> IO Bool
        perDeviceCheck myGdb num = do
          let
            headBaseAddr = gppeBufferReg @Integer
            myBaseAddr = headBaseAddr + 24 * (fromIntegral (L.length muGdbs - num - 1))
            dnaBaseAddr = gppeDnaReg @Integer
          myCounter <- Gdb.readLe @(Unsigned 64) headGppeGdb myBaseAddr
          myDeviceDna2 <- Gdb.readLe @(Unsigned 32) myGdb dnaBaseAddr
          myDeviceDna1 <- Gdb.readLe @(Unsigned 32) myGdb (dnaBaseAddr + 0x4)
          myDeviceDna0 <- Gdb.readLe @(Unsigned 32) myGdb (dnaBaseAddr + 0x8)
          headDeviceDna2 <- Gdb.readLe @(Unsigned 32) headMuGdb (myBaseAddr + 0x08)
          headDeviceDna1 <- Gdb.readLe @(Unsigned 32) headMuGdb (myBaseAddr + 0x0C)
          headDeviceDna0 <- Gdb.readLe @(Unsigned 32) headMuGdb (myBaseAddr + 0x10)
          let
            myCfg = configs L.!! num
            expectedCounter = myCfg.writeMetacycle
            counterEq = myCounter == expectedCounter
            dna0Eq = myDeviceDna0 == headDeviceDna0
            dna1Eq = myDeviceDna1 == headDeviceDna1
            dna2Eq = myDeviceDna2 == headDeviceDna2
          when (not counterEq)
            $ putStrLn
              [i|Counter #{num} did not match. Found #{myCounter}, expected #{expectedCounter}|]
          when (not dna0Eq)
            $ putStrLn
              [i|DNA0 #{num} did not match. Found #{headDeviceDna0}, expected #{myDeviceDna0}|]
          when (not dna1Eq)
            $ putStrLn
              [i|DNA1 #{num} did not match. Found #{headDeviceDna1}, expected #{myDeviceDna1}|]
          when (not dna2Eq)
            $ putStrLn
              [i|DNA2 #{num} did not match. Found #{headDeviceDna2}, expected #{myDeviceDna2}|]
          return $ counterEq && dna0Eq && dna1Eq && dna2Eq

      deviceChecks <- forM (L.zip muGdbs [0 ..]) $ \(gdb, n) -> liftIO $ perDeviceCheck gdb n
      let result = if and deviceChecks then ExitSuccess else ExitFailure 2
      return result

  forM_ targets (assertProbe "probe_test_start")
  tryWithTimeout "Wait for handshakes successes from all boards" 30_000_000
    $ awaitHandshakes targets
  let openOcdStarts = liftIO <$> L.zipWith (initOpenOcd hitlDir) targets [0 ..]
  brackets openOcdStarts (liftIO . (.cleanup)) $ \initOcdsData -> do
    let
      muPorts = (.muPort) <$> initOcdsData
      ccPorts = (.ccPort) <$> initOcdsData
      gppePorts = (.gppePort) <$> initOcdsData
      picocomStarts = liftIO <$> L.zipWith (initPicocom hitlDir) targets [0 ..]
    Gdb.withGdbs (L.length targets) $ \ccGdbs -> do
      liftIO $ do
        zipWithConcurrently3_ (initGdb hitlDir "clock-control") ccGdbs ccPorts targets
        putStrLn "Checking for MMIO access to SwCC CPUs over GDB..."
      gdbExitCodes0 <- mapM (gdbCheck ccWhoAmID) ccGdbs
      (gdbCount0, gdbExitCode0) <-
        L.foldl foldExitCodes (pure (0, ExitSuccess)) gdbExitCodes0
      liftIO $ do
        putStrLn
          [i|CC GDB testing passed on #{gdbCount0} of #{L.length targets} targets|]
        mapConcurrently_ ((errorToException =<<) . Gdb.loadBinary) ccGdbs
      Gdb.withGdbs (L.length targets) $ \muGdbs -> do
        liftIO $ do
          zipWithConcurrently3_ (initGdb hitlDir "management-unit") muGdbs muPorts targets
          putStrLn "Checking for MMIO access to MU CPUs over GDB..."
        gdbExitCodes1 <- mapM (gdbCheck muWhoAmID) muGdbs
        (gdbCount1, gdbExitCode1) <-
          L.foldl foldExitCodes (pure (0, ExitSuccess)) gdbExitCodes1
        liftIO $ do
          putStrLn
            [i|MU GDB testing passed on #{gdbCount1} of #{L.length targets} targets|]
          mapConcurrently_ ((errorToException =<<) . Gdb.loadBinary) muGdbs
        Gdb.withGdbs (L.length targets) $ \gppeGdbs -> do
          liftIO $ do
            zipWithConcurrently3_ (initGdb hitlDir "gppe-switch-demo") gppeGdbs gppePorts targets
            putStrLn "Checking for MMIO access to GPPE CPUs over GDB..."
          gdbExitCodes2 <- mapM (gdbCheck gppeWhoAmID) gppeGdbs
          (gdbCount2, gdbExitCode2) <-
            L.foldl foldExitCodes (pure (0, ExitSuccess)) gdbExitCodes2
          liftIO $ do
            putStrLn
              [i|GPPE GDB testing passed on #{gdbCount2} of #{L.length targets} targets|]
            mapConcurrently_ ((errorToException =<<) . Gdb.loadBinary) gppeGdbs
          brackets picocomStarts (liftIO . snd) $ \(L.map fst -> picocoms) -> do
            let goDumpCcSamples = dumpCcSamples hitlDir (defCcConf (natToNum @FpgaCount)) ccGdbs
            ugnPairsTable <- liftIO $ do
              mapConcurrently_ Gdb.continue ccGdbs
              mapConcurrently_ Gdb.continue muGdbs
              tryWithTimeoutOn "Waiting for stable links" 60_000_000 goDumpCcSamples
                $ forConcurrently_ picocoms
                $ \pico ->
                  waitForLine pico.stdoutHandle "[CC] All links stable"
              putStrLn "Getting UGNs for all targets"
              mapConcurrently_ Gdb.interrupt muGdbs
              zipWithConcurrently muGetUgns targets muGdbs
            let
              ugnPairsTableV = fromJust . V.fromList $ fromJust . V.fromList <$> ugnPairsTable
            currentTime <- liftIO $ do
              putStrLn "Calculating IGNs for all targets"
              Calc.printAllIgns ugnPairsTableV fpgaSetup
              mapM_ print ugnPairsTableV
              muGetCurrentTime (L.head targets) (L.head muGdbs)
            let
              startOffset = currentTime + natToNum @(PeriodToCycles GthTx (Seconds StartDelay))
              metaChainConfig ::
                Vec FpgaCount (Calc.DefaultGppeMetaPeConfig (Unsigned 64) FpgaCount 3 Padding)
              metaChainConfig =
                Calc.fullChainConfiguration gppeConfig fpgaSetup ugnPairsTableV startOffset
            liftIO $ do
              putStrLn [i|Starting clock cycle: #{startOffset}|]
              putStrLn [i|Cycles per write: #{natToNum @CyclesPerWrite :: Integer}|]
              putStrLn [i|Cycles per group: #{natToNum @GroupCycles :: Integer}|]
              putStrLn [i|Cycles per window: #{natToNum @WindowCycles :: Integer}|]
              putStrLn [i|Cycles per active period: #{natToNum @ActiveCycles :: Integer}|]
              putStrLn [i|Cycles of padding: #{natToNum @Padding :: Integer}|]
              putStrLn [i|Cycles per metacycle: #{natToNum @MetacycleLength :: Integer}|]
              putStrLn "Calculated the following configs for the switch processing elements:"
              forM_ metaChainConfig print
            _ <- sequenceA $ L.zipWith3 gppeWriteCfg targets gppeGdbs (toList metaChainConfig)

            let delayMicros = natToNum @StartDelay * 1_250_000
            _ <- liftIO $ do
              mapConcurrently_ Gdb.continue gppeGdbs
              threadDelay delayMicros
              putStrLn [i|Slept for: #{delayMicros}μs|]
              newCurrentTime <- muGetCurrentTime (L.head targets) (L.head muGdbs)
              putStrLn [i|Clock is now: #{newCurrentTime}|]
              _ <- sequenceA $ Gdb.interrupt <$> gppeGdbs
              sequenceA $ L.zipWith gppeReadBuffer targets gppeGdbs

            bufferExit <- finalCheck muGdbs gppeGdbs (toList metaChainConfig)

            liftIO goDumpCcSamples

            let
              finalExit =
                fromMaybe ExitSuccess
                  $ L.find (/= ExitSuccess) [gdbExitCode0, gdbExitCode1, gdbExitCode2, bufferExit]
            return finalExit
