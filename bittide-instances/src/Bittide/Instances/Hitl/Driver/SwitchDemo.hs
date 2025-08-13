-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
-- TODO: Remove use of partial functions
{-# OPTIONS_GHC -Wno-x-partial #-}

module Bittide.Instances.Hitl.Driver.SwitchDemo where

import Clash.Prelude

import Bittide.ClockControl.Config (CcConf, defCcConf, saveCcConfig)
import Bittide.ClockControl.Topology (Topology)
import Bittide.Hitl
import Bittide.Instances.Domains
import Bittide.Instances.Hitl.Setup (FpgaCount, fpgaSetup)
import Bittide.Instances.Hitl.SwitchDemo (memoryMapCc, memoryMapMu)
import Bittide.Instances.Hitl.Utils.Driver
import Bittide.Instances.Hitl.Utils.Program
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently_, mapConcurrently_)
import Control.Concurrent.Async.Extra (zipWithConcurrently, zipWithConcurrently3_)
import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class
import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (isDigit)
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
import Text.Read (readMaybe)
import Text.Show.Pretty (ppShow)
import Vivado.Tcl (HwTarget)
import Vivado.VivadoM
import "bittide-extra" Control.Exception.Extra (brackets)
import "extra" Data.List.Extra (trim)

import qualified Bittide.Calculator as Calc
import qualified Bittide.Instances.Hitl.Utils.OpenOcd as Ocd
import qualified Bittide.Instances.Hitl.Utils.Picocom as Picocom
import qualified Clash.Sized.Vector as V (fromList)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Gdb

data OcdInitData = OcdInitData
  { muPort :: Int
  -- ^ Management unit GDB port
  , ccPort :: Int
  -- ^ Clock control GDB port
  , handles :: ProcessHandles
  -- ^ OpenOCD stdio handles
  , cleanup :: IO ()
  -- ^ Cleanup function
  }

data TestStatus = TestRunning | TestDone Bool | TestTimeout deriving (Eq)

type StartDelay = 5 -- seconds

type Padding = Calc.WindowCycles FpgaCount 3
type GppeConfig = Calc.DefaultGppeConfig FpgaCount Padding
type CyclesPerWrite = Calc.CalCyclesPerWrite GppeConfig
type GroupCycles = Calc.CalGroupCycles GppeConfig
type WindowCycles = Calc.CalWindowCycles GppeConfig
type ActiveCycles = Calc.CalActiveCycles GppeConfig
type MetacycleLength = Calc.CalMetacycleLength GppeConfig

gppeConfig :: GppeConfig
gppeConfig = Calc.defaultGppeCalcConfig

{- | Compare two hexadecimal strings for equality. Given strings do not have to
start with \"0x\". The comparison is case-insensitive, though the prefix must
be exactly \"0x\" if it is present (not \"0X\").

>>> hexEq "0xAB" "0xab"
True
>>> hexEq "0xAB" "0xabc"
False
>>> hexEq "0x0AB" "0xAB"
True
>>> hexEq "AB" "0xAB"
True
-}
hexEq :: String -> String -> Bool
hexEq a0 b0 =
  case (readMaybe @Integer a1, readMaybe b1) of
    (Just a, Just b) -> a == b
    _ -> False
 where
  a1 = "0x" <> dropPrefix "0x" a0
  b1 = "0x" <> dropPrefix "0x" b0

-- | Drop a prefix from a list if it exists
dropPrefix :: (Eq a) => [a] -> [a] -> [a]
dropPrefix prefix xs
  | prefix `L.isPrefixOf` xs = L.drop (L.length prefix) xs
  | otherwise = xs

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
ccWhoAmIReg :: (HasCallStack, Num a, Show a) => a
ccWhoAmIReg = getCcRegisterAddress "0/WhoAmI.identifier"

muAnnAbsTree :: MemoryMapTreeAbsNorm
muAnnAbsTree = getMemoryMapTreeAbsNorm memoryMapMu

muRegisterPaths :: (HasCallStack, Num a, Show a) => [(String, a)]
muRegisterPaths = enumerateRegisters memoryMapMu.deviceDefs muAnnAbsTree

getMuRegisterAddress :: (HasCallStack, Num a, Show a) => String -> a
getMuRegisterAddress = getRegisterAddress muRegisterPaths

muCaptureUgnDevs :: (HasCallStack, Num a, Show a) => [a]
muCaptureUgnDevs =
  if L.length captureUgnDevs == (natToNum @FpgaCount) - 1
    then snd <$> captureUgnDevs
    else error [i|Found only #{L.length captureUgnDevs} `CaptureUgn` devices.|]
 where
  captureUgnDevs =
    L.filter (\(p, _) -> L.isPrefixOf "0/CaptureUgn" p && isDigit (L.last p)) muRegisterPaths
muDnaReg :: (HasCallStack, Num a, Show a) => a
muDnaReg = getMuRegisterAddress "0/Dna.dna"
muSwitchDemoPeBuffer :: (HasCallStack, Num a, Show a) => a
muSwitchDemoPeBuffer = getMuRegisterAddress "0/SwitchDemoPE.buffer"
muSwitchDemoPeReadCycles :: (HasCallStack, Num a, Show a) => a
muSwitchDemoPeReadCycles = getMuRegisterAddress "0/SwitchDemoPE.read_cycles"
muSwitchDemoPeReadStart :: (HasCallStack, Num a, Show a) => a
muSwitchDemoPeReadStart = getMuRegisterAddress "0/SwitchDemoPE.read_start"
muSwitchDemoPeWriteCycles :: (HasCallStack, Num a, Show a) => a
muSwitchDemoPeWriteCycles = getMuRegisterAddress "0/SwitchDemoPE.write_cycles"
muSwitchDemoPeWriteStart :: (HasCallStack, Num a, Show a) => a
muSwitchDemoPeWriteStart = getMuRegisterAddress "0/SwitchDemoPE.write_start"
muTimerCmdReg :: (HasCallStack, Num a, Show a) => a
muTimerCmdReg = getMuRegisterAddress "0/Timer.command"
muTimerScratchpadReg :: (HasCallStack, Num a, Show a) => a
muTimerScratchpadReg = getMuRegisterAddress "0/Timer.scratchpad"
muWhoAmIReg :: (HasCallStack, Num a, Show a) => a
muWhoAmIReg = getMuRegisterAddress "0/WhoAmI.identifier"

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
    nSamplesWritten <-
      Gdb.readSingleGdbValue
        gdb
        ("N_SAMPLES_WRITTEN")
        ("x/1wx " <> showHex32 sampleMemoryBase)

    let
      bytesPerSample = 13
      bytesPerWord = 4

      dumpStart = sampleMemoryBase + bytesPerWord
      dumpEnd n = dumpStart + fromIntegral n * bytesPerWord * bytesPerSample

    case readMaybe nSamplesWritten of
      Nothing -> error [i|Could not parse GDB output as number: #{nSamplesWritten}|]
      Just n -> Gdb.dumpMemoryRegion gdb dumpPath dumpStart (dumpEnd n) >> pure n

  sampleMemoryBase = ccSampleMemoryReg @Integer
  ccSamplesPaths = [[i|#{hitlDir}/cc-samples-#{n}.bin|] | n <- [(0 :: Int) .. 7]]

initOpenOcd :: FilePath -> (HwTarget, DeviceInfo) -> Int -> IO OcdInitData
initOpenOcd hitlDir (_, d) targetIndex = do
  putStrLn $ "Starting OpenOCD for target " <> d.deviceId

  let
    gdbPortMU = 3333 + targetIndex * 2
    gdbPortCC = gdbPortMU + 1
    tclPortMU = 6666 + targetIndex * 2
    tclPortCC = tclPortMU + 1
    telnetPortMU = 4444 + targetIndex * 2
    telnetPortCC = telnetPortMU + 1
    ocdStdout = hitlDir </> "openocd-" <> show targetIndex <> "-stdout.log"
    ocdStderr = hitlDir </> "openocd-" <> show targetIndex <> "-stderr.log"
  putStrLn $ "logging OpenOCD stdout to `" <> ocdStdout <> "`"
  putStrLn $ "logging OpenOCD stderr to `" <> ocdStderr <> "`"

  putStrLn "Starting OpenOCD..."
  (ocd, ocdPh, ocdClean0) <-
    Ocd.startOpenOcdWithEnvAndArgs
      ["-f", "sipeed.tcl", "-f", "vexriscv-2chain.tcl"]
      [ ("OPENOCD_STDOUT_LOG", ocdStdout)
      , ("OPENOCD_STDERR_LOG", ocdStderr)
      , ("USB_DEVICE", d.usbAdapterLocation)
      , ("DEV_A_GDB", show gdbPortMU)
      , ("DEV_B_GDB", show gdbPortCC)
      , ("DEV_A_TCL", show tclPortMU)
      , ("DEV_B_TCL", show tclPortCC)
      , ("DEV_A_TEL", show telnetPortMU)
      , ("DEV_B_TEL", show telnetPortCC)
      ]
  hSetBuffering ocd.stderrHandle LineBuffering
  tryWithTimeout "Waiting for OpenOCD to start" 15_000_000
    $ expectLine ocd.stderrHandle Ocd.waitForHalt

  let
    ocdProcName = "OpenOCD (" <> d.deviceId <> ")"
    ocdClean1 = ocdClean0 >> awaitProcessTermination ocdProcName ocdPh (Just 10_000_000)

  return $ OcdInitData gdbPortMU gdbPortCC ocd ocdClean1

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

ccGdbCheck :: Gdb -> VivadoM ExitCode
ccGdbCheck gdb = do
  let whoAmIBase = showHex32 @Integer ccWhoAmIReg
  output <- liftIO $ Gdb.readCommand1 gdb [i|x/4cb #{whoAmIBase}|]
  pure
    $ if output == whoAmIBase <> ":\t115 's'\t119 'w'\t99 'c'\t99 'c'"
      then ExitSuccess
      else ExitFailure 1

muGdbCheck :: Gdb -> VivadoM ExitCode
muGdbCheck gdb = do
  let whoAmIBase = showHex32 @Integer muWhoAmIReg
  output <- liftIO $ Gdb.readCommand1 gdb [i|x/4cb #{whoAmIBase}|]
  pure
    $ if output == whoAmIBase <> ":\t109 'm'\t103 'g'\t109 'm'\t116 't'"
      then ExitSuccess
      else ExitFailure 1

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

    muGetUgns ::
      (HwTarget, DeviceInfo) -> Gdb -> IO [(Unsigned 64, Unsigned 64)]
    muGetUgns (_, d) gdb = do
      let
        mmioAddrs :: [String]
        mmioAddrs = L.map (showHex32 @Integer) muCaptureUgnDevs

        readUgnMmio :: String -> IO (Unsigned 64, Unsigned 64)
        readUgnMmio addr = do
          output <- Gdb.readCommand1 gdb [i|x/2xg #{addr}|]
          let outputDataWords = L.words output
          case L.drop (L.length outputDataWords - 2) outputDataWords of
            [read -> txIJ, read -> rxIJ] -> return (txIJ, rxIJ)
            other -> fail [i|Could not read output data. Line: '#{output}', data: #{other}|]

      liftIO $ putStrLn $ "Getting UGNs for device " <> d.deviceId
      liftIO $ mapM readUgnMmio mmioAddrs

    muReadPeBuffer :: (HasCallStack) => (HwTarget, DeviceInfo) -> Gdb -> IO ()
    muReadPeBuffer (_, d) gdb = do
      putStrLn $ "Reading PE buffer from device " <> d.deviceId
      let
        bufferSize :: Integer
        bufferSize = (snatToNum (SNat @FpgaCount)) * 3
      output <-
        Gdb.readCommandRaw gdb [i|x/#{bufferSize}xg #{showHex32 @Integer muSwitchDemoPeBuffer}|]
      putStrLn [i|PE buffer readout: #{output}|]

    muGetCurrentTime ::
      (HasCallStack) =>
      (HwTarget, DeviceInfo) ->
      Gdb ->
      IO (Unsigned 64)
    muGetCurrentTime (_, d) gdb = do
      putStrLn $ "Getting current time from device " <> d.deviceId
      -- Write capture command to `timeWb` component
      Gdb.runCommand gdb [i|set {char[4]}(#{showHex32 @Integer muTimerCmdReg}) = 0x0|]
      currentStringLsbs0 <-
        Gdb.readCommand1 gdb [i|x/1xw #{showHex32 @Integer muTimerScratchpadReg}|]
      let
        currentStringLsbs1 = trim $ L.head $ lines $ L.drop 2 $ L.dropWhile (/= ':') currentStringLsbs0
        currentTimeLsbsI :: Integer
        currentTimeLsbsI = read currentStringLsbs1
        currentTimeLsbs :: Unsigned 32
        currentTimeLsbs = fromIntegral currentTimeLsbsI
      putStrLn $ "GDB said: " <> show currentStringLsbs1
      putStrLn $ "I read: " <> show currentTimeLsbs
      currentStringMsbs0 <-
        Gdb.readCommand1 gdb [i|x/1xw #{showHex32 (muTimerScratchpadReg @Integer + 0x4)}|]
      let
        currentStringMsbs1 = trim $ L.head $ lines $ L.drop 2 $ L.dropWhile (/= ':') currentStringMsbs0
        currentTimeMsbsI :: Integer
        currentTimeMsbsI = read currentStringMsbs1
        currentTimeMsbs :: Unsigned 32
        currentTimeMsbs = fromIntegral currentTimeMsbsI
      putStrLn $ "GDB said: " <> currentStringMsbs1
      putStrLn $ "I read: " <> show currentTimeMsbs
      return $ bitCoerce (currentTimeMsbs, currentTimeLsbs)

    muWriteCfg ::
      (HasCallStack) =>
      (HwTarget, DeviceInfo) ->
      Gdb ->
      Calc.CyclePeConfig (Unsigned 64) (Index 9) ->
      VivadoM ()
    muWriteCfg target@(_, d) gdb (bimap pack fromIntegral -> cfg) = do
      let
        write64 :: Unsigned 32 -> BitVector 64 -> [String]
        write64 address value =
          [ [i|set {char[4]}(#{showHex32 address}) = #{showHex32 valueLsb}|]
          , [i|set {char[4]}(#{showHex32 (address + 4)}) = #{showHex32 valueMsb}|]
          ]
         where
          (valueMsb :: BitVector 32, valueLsb :: BitVector 32) = bitCoerce value

      liftIO $ do
        muReadPeBuffer target gdb
        putStrLn $ "Writing config to device " <> d.deviceId
        Gdb.runCommands
          gdb
          ( L.concat
              [ write64 muSwitchDemoPeReadStart cfg.startReadAt
              , write64 muSwitchDemoPeReadCycles cfg.readForN
              , write64 muSwitchDemoPeWriteStart cfg.startWriteAt
              , write64 muSwitchDemoPeWriteCycles cfg.writeForN
              ]
          )

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
            myBaseAddr = headBaseAddr + 24 * (L.length gdbs - num - 1)
            dnaBaseAddr = muDnaReg @Integer
          myCounter <-
            Gdb.readSingleGdbValue headGdb ("COUNTER-" <> show num) ("x/1gx " <> showHex32 myBaseAddr)
          myDeviceDna2 <-
            Gdb.readSingleGdbValue myGdb ("DNA2-" <> show num) [i|x/1wx #{showHex32 dnaBaseAddr}|]
          myDeviceDna1 <-
            Gdb.readSingleGdbValue
              myGdb
              ("DNA1-" <> show num)
              [i|x/1wx #{showHex32 (dnaBaseAddr + 0x4)}|]
          myDeviceDna0 <-
            Gdb.readSingleGdbValue
              myGdb
              ("DNA0-" <> show num)
              [i|x/1wx #{showHex32 (dnaBaseAddr + 0x8)}|]
          headDeviceDna2 <-
            Gdb.readSingleGdbValue
              headGdb
              ("DNA2-" <> show num)
              ("x/1wx " <> showHex32 (myBaseAddr + 0x08))
          headDeviceDna1 <-
            Gdb.readSingleGdbValue
              headGdb
              ("DNA2-" <> show num)
              ("x/1wx " <> showHex32 (myBaseAddr + 0x0C))
          headDeviceDna0 <-
            Gdb.readSingleGdbValue
              headGdb
              ("DNA2-" <> show num)
              ("x/1wx " <> showHex32 (myBaseAddr + 0x10))
          let
            myCfg = configs L.!! num
            expectedCounter = showHex myCfg.startWriteAt ""
            counterEq = hexEq myCounter expectedCounter
            dna0Eq = hexEq myDeviceDna0 headDeviceDna0
            dna1Eq = hexEq myDeviceDna1 headDeviceDna1
            dna2Eq = hexEq myDeviceDna2 headDeviceDna2
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

      deviceChecks <- forM (L.zip gdbs [0 ..]) $ \(gdb, n) -> liftIO $ perDeviceCheck gdb n
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
    Gdb.withGdbs (L.length targets) $ \ccGdbs -> do
      liftIO $ zipWithConcurrently3_ (initGdb hitlDir "clock-control") ccGdbs ccPorts targets
      liftIO $ putStrLn "Checking for MMIO access to SwCC CPUs over GDB..."
      gdbExitCodes0 <- mapM ccGdbCheck ccGdbs
      (gdbCount0, gdbExitCode0) <-
        L.foldl foldExitCodes (pure (0, ExitSuccess)) gdbExitCodes0
      liftIO
        $ putStrLn
          [i|CC GDB testing passed on #{gdbCount0} of #{L.length targets} targets|]
      liftIO $ mapConcurrently_ ((errorToException =<<) . Gdb.loadBinary) ccGdbs

      Gdb.withGdbs (L.length targets) $ \muGdbs -> do
        liftIO $ zipWithConcurrently3_ (initGdb hitlDir "management-unit") muGdbs muPorts targets
        liftIO $ putStrLn "Checking for MMIO access to MU CPUs over GDB..."
        gdbExitCodes1 <- mapM muGdbCheck muGdbs
        (gdbCount1, gdbExitCode1) <-
          L.foldl foldExitCodes (pure (0, ExitSuccess)) gdbExitCodes1
        liftIO
          $ putStrLn
            [i|MU GDB testing passed on #{gdbCount1} of #{L.length targets} targets|]
        liftIO $ mapConcurrently_ ((errorToException =<<) . Gdb.loadBinary) muGdbs

        let picocomStarts = liftIO <$> L.zipWith (initPicocom hitlDir) targets [0 ..]
        brackets picocomStarts (liftIO . snd) $ \(L.map fst -> picocoms) -> do
          let goDumpCcSamples = dumpCcSamples hitlDir (defCcConf (natToNum @FpgaCount)) ccGdbs
          liftIO $ mapConcurrently_ Gdb.continue ccGdbs
          liftIO $ mapConcurrently_ Gdb.continue muGdbs
          liftIO
            $ tryWithTimeoutOn "Waiting for stable links" 60_000_000 goDumpCcSamples
            $ forConcurrently_ picocoms
            $ \pico ->
              waitForLine pico.stdoutHandle "[CC] All links stable"

          liftIO $ putStrLn "Getting UGNs for all targets"
          liftIO $ mapConcurrently_ Gdb.interrupt muGdbs
          ugnPairsTable <- liftIO $ zipWithConcurrently muGetUgns targets muGdbs
          let
            ugnPairsTableV = fromJust . V.fromList $ fromJust . V.fromList <$> ugnPairsTable
          liftIO $ do
            putStrLn "Calculating IGNs for all targets"
            Calc.printAllIgns ugnPairsTableV fpgaSetup
            mapM_ print ugnPairsTableV
          currentTime <- liftIO $ muGetCurrentTime (L.head targets) (L.head muGdbs)
          let
            startOffset = currentTime + natToNum @(PeriodToCycles GthTx (Seconds StartDelay))
            metaChainConfig ::
              Vec FpgaCount (Calc.DefaultGppeMetaPeConfig (Unsigned 64) FpgaCount 3 Padding)
            metaChainConfig =
              Calc.fullChainConfiguration gppeConfig fpgaSetup ugnPairsTableV startOffset
            chainConfig :: Vec FpgaCount (Calc.CyclePeConfig (Unsigned 64) (Index (FpgaCount + 1)))
            chainConfig =
              Calc.metaPeConfigToCyclePeConfig (natToNum @MetacycleLength)
                <$> metaChainConfig
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
            forM_ chainConfig print
          _ <- sequenceA $ L.zipWith3 muWriteCfg targets muGdbs (toList chainConfig)
          liftIO $ do
            let delayMicros = natToNum @StartDelay * 1_250_000
            threadDelay delayMicros
            putStrLn [i|Slept for: #{delayMicros}μs|]
            newCurrentTime <- muGetCurrentTime (L.head targets) (L.head muGdbs)
            putStrLn [i|Clock is now: #{newCurrentTime}|]

          _ <- liftIO $ sequenceA $ L.zipWith muReadPeBuffer targets muGdbs

          bufferExit <- finalCheck muGdbs (toList chainConfig)

          liftIO goDumpCcSamples

          pure
            $ fromMaybe ExitSuccess
            $ L.find (/= ExitSuccess) [gdbExitCode0, gdbExitCode1, bufferExit]
