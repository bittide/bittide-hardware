-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}
-- TODO: Remove use of partial functions
{-# OPTIONS_GHC -Wno-x-partial #-}

module Bittide.Instances.Hitl.Driver.SwitchDemo where

import Clash.Prelude

import Bittide.Hitl
import Bittide.Instances.Domains
import Bittide.Instances.Hitl.Setup (FpgaCount, fpgaSetup)
import Bittide.Instances.Hitl.SwitchDemo (memoryMapCc, memoryMapMu)
import Bittide.Instances.Hitl.Utils.Driver
import Bittide.Instances.Hitl.Utils.Program
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently_)
import Control.Concurrent.Async.Extra (zipWithConcurrently)
import Control.Monad (forM, forM_, when, zipWithM)
import Control.Monad.IO.Class
import Data.Bifunctor (Bifunctor (bimap))
import Data.Maybe (fromJust, fromMaybe)
import Data.String.Interpolate (i)
import GHC.Stack (HasCallStack)
import Numeric (showHex)
import Project.FilePath
import Project.Handle
import Protocols.MemoryMap (MemoryMap (..), MemoryMapTree (DeviceInstance, Interconnect))
import System.Exit
import System.FilePath
import System.IO
import System.Process (StdStream (CreatePipe, UseHandle), callProcess)
import Text.Read (readMaybe)
import Text.Show.Pretty (ppShow)
import Vivado.Tcl (HwTarget)
import Vivado.VivadoM
import "extra" Data.List.Extra (trim)

import qualified Bittide.Calculator as Calc
import qualified Bittide.Instances.Hitl.Utils.Gdb as Gdb
import qualified Bittide.Instances.Hitl.Utils.OpenOcd as Ocd
import qualified Bittide.Instances.Hitl.Utils.Picocom as Picocom
import qualified Clash.Sized.Vector as V (fromList)
import qualified Data.List as L

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

{- | Get base addresses of a device from a memory map. Assumes that all devices
are located at a single interconnect, which is a bit grimy.
-}
baseAddresses :: (HasCallStack, Num a) => MemoryMap -> String -> [a]
baseAddresses memoryMap nm =
  case memoryMap of
    MemoryMap{tree = Interconnect _ devices} ->
      [fromIntegral addr | (addr, DeviceInstance _ d) <- devices, d == nm]
    _ -> error [i|Unexpected memory map structure: #{ppShow memoryMapMu}|]

{- | Like 'baseAddresses', but assume that there is only one device with the
given name in the memory map. If there are multiple devices with the same
name, an error is raised.
-}
baseAddress :: (HasCallStack, Num a, Show a) => MemoryMap -> String -> a
baseAddress memoryMap nm =
  case baseAddresses memoryMap nm of
    [addr] -> addr
    addrs -> error [i|Expected a single base address for #{nm}, got: #{show addrs}|]

-- | Like 'baseAddress', but specialized on the management unit memory map.
muBaseAddress :: (HasCallStack, Num a, Show a) => String -> a
muBaseAddress = baseAddress memoryMapMu

-- | Like 'baseAddress', but specialized on the clock control memory map.
ccBaseAddress :: (HasCallStack, Num a, Show a) => String -> a
ccBaseAddress = baseAddress memoryMapCc

{- | Addresses of the capture UGN devices in the management unit. As a sanity
check, it is expected that there are exactly 7 capture UGN devices present.
-}
muCaptureUgnAddresses :: (HasCallStack, Num a, Show a) => [a]
muCaptureUgnAddresses =
  if L.length ugnDevices == 7
    then ugnDevices
    else error $ "Expected 7 capture ugn devices, got: " <> ppShow ugnDevices
 where
  ugnDevices = baseAddresses memoryMapMu "CaptureUgn"

dumpCcSamples :: (HasCallStack) => FilePath -> [ProcessHandles] -> IO [Word]
dumpCcSamples hitlDir ccGdbs = do
  mapM_ Gdb.interrupt ccGdbs
  nSamples <- liftIO $ zipWithConcurrently go ccGdbs ccSamplesPaths
  putStrLn [i|Dumped /n/ clock control samples: #{nSamples}|]
  pure nSamples
 where
  go :: (HasCallStack) => ProcessHandles -> FilePath -> IO Word
  go gdb dumpPath = do
    nSamplesWritten <-
      readSingleGdbValue
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

  sampleMemoryBase = ccBaseAddress @Integer "SampleMemory"
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
  Int ->
  (HwTarget, DeviceInfo) ->
  IO (ProcessHandles, IO ())
initGdb hitlDir binName gdbPort (hwT, d) = do
  putStrLn $ "Starting GDB for target " <> d.deviceId <> " with bin name " <> binName

  (gdb, gdbPh, gdbClean0) <- Gdb.startGdbH
  hSetBuffering gdb.stdinHandle LineBuffering
  hSetBuffering gdb.stdoutHandle LineBuffering

  Gdb.setLogging gdb $ hitlDir
    </> "gdb-" <> binName <> "-" <> show (getTargetIndex hwT) <> ".log"
  Gdb.setFile gdb $ firmwareBinariesDir "riscv32imc" Release </> binName
  Gdb.setTarget gdb gdbPort
  Gdb.setTimeout gdb Nothing
  Gdb.runCommands gdb.stdinHandle ["echo connected to target device"]

  let
    gdbProcName = "GDB (" <> binName <> ", " <> d.deviceId <> ")"
    gdbClean1 = gdbClean0 >> awaitProcessTermination gdbProcName gdbPh (Just 10_000_000)

  return (gdb, gdbClean1)

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

ccGdbCheck :: ProcessHandles -> VivadoM ExitCode
ccGdbCheck gdb = do
  liftIO
    $ Gdb.runCommands
      gdb.stdinHandle
      [ "echo START OF WHOAMI\\n"
      , "x/4cb " <> whoAmIBase
      , "echo END OF WHOAMI\\n"
      ]
  _ <-
    liftIO
      $ tryWithTimeout "Waiting for GDB to be ready to proceed" 15_000_000
      $ readUntil gdb.stdoutHandle "START OF WHOAMI"
  gdbRead <-
    liftIO
      $ tryWithTimeout "Reading CC whoami over GDB" 15_000_000
      $ readUntil gdb.stdoutHandle "END OF WHOAMI"
  let
    idLine = trim . L.head . lines $ trim gdbRead
    success = idLine == "(gdb) " <> whoAmIBase <> ":\t115 's'\t119 'w'\t99 'c'\t99 'c'"
  liftIO $ putStrLn [i|Output from CC whoami probe:\n#{idLine}|]
  return $ if success then ExitSuccess else ExitFailure 1
 where
  whoAmIBase = showHex32 $ ccBaseAddress @Integer "WhoAmI"

muGdbCheck :: ProcessHandles -> VivadoM ExitCode
muGdbCheck gdb = do
  liftIO
    $ Gdb.runCommands
      gdb.stdinHandle
      [ "echo START OF WHOAMI\\n"
      , "x/4cb " <> whoAmIBase
      , "echo END OF WHOAMI\\n"
      ]
  _ <-
    liftIO
      $ tryWithTimeout "Waiting for GDB to be ready to proceed" 15_000_000
      $ readUntil gdb.stdoutHandle "START OF WHOAMI"
  gdbRead <-
    liftIO
      $ tryWithTimeout "Reading MU whoami over GDB" 15_000_000
      $ readUntil gdb.stdoutHandle "END OF WHOAMI"
  let
    idLine = trim . L.head . lines $ trim gdbRead
    success = idLine == "(gdb) " <> whoAmIBase <> ":\t109 'm'\t103 'g'\t109 'm'\t116 't'"
  liftIO $ putStrLn [i|Output from MU whoami probe:\n#{idLine}|]
  return $ if success then ExitSuccess else ExitFailure 1
 where
  whoAmIBase = showHex32 $ muBaseAddress @Integer "WhoAmI"

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
    plotDataDumpPath =
      projectDir </> "bittide-instances" </> "data" </> "plot" </> "plot_data_dump.py"
    hitlDir = projectDir </> "_build/hitl" </> testName

    muGetUgns ::
      (HwTarget, DeviceInfo) -> ProcessHandles -> VivadoM [(Unsigned 64, Unsigned 64)]
    muGetUgns (_, d) gdb = do
      let
        mmioAddrs :: [String]
        mmioAddrs = L.map (showHex32 @Integer) muCaptureUgnAddresses

        readUgnMmio :: String -> IO (Unsigned 64, Unsigned 64)
        readUgnMmio addr = do
          let
            startString = "START OF UGN (" <> addr <> ")"
            endString = "END OF UGN (" <> addr <> ")"
          Gdb.runCommands
            gdb.stdinHandle
            [ "printf \"" <> startString <> "\\n\""
            , "x/2xg " <> addr
            , "printf \"" <> endString <> "\\n\""
            ]
          _ <-
            tryWithTimeout "Waiting for GDB to be ready for UGN readout" 15_000_000
              $ readUntil gdb.stdoutHandle startString
          gdbRead <-
            tryWithTimeout "Reading UGN over GDB" 15_000_000 $ readUntil gdb.stdoutHandle endString
          let
            outputLine = L.head $ L.lines $ trim gdbRead
            outputDataWords = L.words $ trim $ outputLine
          putStrLn $ "GDB output: {{\n" <> gdbRead <> "}}"
          case L.drop (L.length outputDataWords - 2) outputDataWords of
            [read -> txIJ, read -> rxIJ] -> return (txIJ, rxIJ)
            other -> fail [i|Could not read output data. Line: '#{outputLine}', data: #{other}|]

      liftIO $ putStrLn $ "Getting UGNs for device " <> d.deviceId
      liftIO $ mapM readUgnMmio mmioAddrs

    muReadPeBuffer :: (HasCallStack) => (HwTarget, DeviceInfo) -> ProcessHandles -> IO ()
    muReadPeBuffer (_, d) gdb = do
      putStrLn $ "Reading PE buffer from device " <> d.deviceId
      let
        start = muBaseAddress @Integer "SwitchDemoPE"
        startString = "START OF PEBUF (" <> d.deviceId <> ")"
        endString = "END OF PEBUF (" <> d.deviceId <> ")"
        bufferSize :: Integer
        bufferSize = (snatToNum (SNat @FpgaCount)) * 3
      Gdb.runCommands
        gdb.stdinHandle
        [ "printf \"" <> startString <> "\\n\""
        , [i|x/#{bufferSize}xg #{showHex32 (start + 0x28)}|]
        , "printf \"" <> endString <> "\\n\""
        ]
      _ <-
        tryWithTimeout "Waiting for GDB to be ready for PE buffer readout" 15_000_000
          $ readUntil gdb.stdoutHandle startString
      bufInit <-
        tryWithTimeout "PE buffer readout" 15_000_000 $ readUntil gdb.stdoutHandle endString
      putStrLn $ "PE buffer readout:\n" <> bufInit

    muGetCurrentTime ::
      (HasCallStack) =>
      (HwTarget, DeviceInfo) ->
      ProcessHandles ->
      IO (Unsigned 64)
    muGetCurrentTime (_, d) gdb = do
      putStrLn $ "Getting current time from device " <> d.deviceId
      let timerBase = muBaseAddress @Integer "Timer"
      -- Write capture command to `timeWb` component
      Gdb.runCommands gdb.stdinHandle [[i|set {char[4]}(#{showHex32 timerBase}) = 0x0|]]
      let
        currentStringLsbs = "START OF STARTTIME LSBS"
        endStringLsbs = "END OF STARTTIME LSBS"
      Gdb.runCommands
        gdb.stdinHandle
        [ "printf \"" <> currentStringLsbs <> "\\n\""
        , [i|x/1xw #{showHex32 (timerBase + 0x8)}|]
        , "printf \"" <> endStringLsbs <> "\\n\""
        ]
      _ <-
        tryWithTimeout "Waiting for GDB to be ready for curtime lsbs readout" 15_000_000
          $ readUntil gdb.stdoutHandle currentStringLsbs
      currentStringLsbs0 <-
        tryWithTimeout "Current time lsbs readout" 15_000_000
          $ readUntil gdb.stdoutHandle endStringLsbs
      let
        currentStringLsbs1 = trim $ L.head $ lines $ L.drop 2 $ L.dropWhile (/= ':') currentStringLsbs0
        currentTimeLsbsI :: Integer
        currentTimeLsbsI = read currentStringLsbs1
        currentTimeLsbs :: Unsigned 32
        currentTimeLsbs = fromIntegral currentTimeLsbsI
      putStrLn $ "GDB said: " <> show currentStringLsbs1
      putStrLn $ "I read: " <> show currentTimeLsbs
      let
        currentStringMsbs = "START OF CURTIME MSBS"
        endStringMsbs = "END OF CURTIME MSBS"
      Gdb.runCommands
        gdb.stdinHandle
        [ "printf \"" <> currentStringMsbs <> "\\n\""
        , [i|x/1xw #{showHex32 (timerBase + 0xC)}|]
        , "printf \"" <> endStringMsbs <> "\\n\""
        ]
      _ <-
        tryWithTimeout "Waiting for GDB to be ready for curtime msbs readout" 15_000_000
          $ readUntil gdb.stdoutHandle currentStringMsbs
      currentStringMsbs0 <-
        tryWithTimeout "Current time msbs readout" 15_000_000
          $ readUntil gdb.stdoutHandle endStringMsbs
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
      ProcessHandles ->
      Calc.CyclePeConfig (Unsigned 64) (Index 9) ->
      VivadoM ()
    muWriteCfg target@(_, d) gdb (bimap pack fromIntegral -> cfg) = do
      let
        start = muBaseAddress "SwitchDemoPE"

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
          gdb.stdinHandle
          ( L.concat
              [ write64 (start + 0) cfg.startReadAt
              , write64 (start + 8) cfg.readForN
              , write64 (start + 16) cfg.startWriteAt
              , write64 (start + 24) cfg.writeForN
              ]
          )

    finalCheck ::
      (HasCallStack) =>
      [ProcessHandles] ->
      [Calc.CyclePeConfig (Unsigned 64) (Index 9)] ->
      VivadoM ExitCode
    finalCheck [] _ = fail "Should pass in two or more GDBs for the final check!"
    finalCheck (_ : []) _ = fail "Should pass in two or more GDBs for the final check!"
    finalCheck gdbs@(headGdb : _) configs = do
      let
        perDeviceCheck :: ProcessHandles -> Int -> IO Bool
        perDeviceCheck myGdb num = do
          let
            headBaseAddr = muBaseAddress "SwitchDemoPE" + 0x28
            myBaseAddr = headBaseAddr + 24 * (L.length gdbs - num - 1)
            dnaBaseAddr = muBaseAddress @Integer "Dna"
          myCounter <-
            readSingleGdbValue headGdb ("COUNTER-" <> show num) ("x/1gx " <> showHex32 myBaseAddr)
          myDeviceDna2 <-
            readSingleGdbValue myGdb ("DNA2-" <> show num) [i|x/1wx #{showHex32 dnaBaseAddr}|]
          myDeviceDna1 <-
            readSingleGdbValue myGdb ("DNA1-" <> show num) [i|x/1wx #{showHex32 (dnaBaseAddr + 0x4)}|]
          myDeviceDna0 <-
            readSingleGdbValue myGdb ("DNA0-" <> show num) [i|x/1wx #{showHex32 (dnaBaseAddr + 0x8)}|]
          headDeviceDna2 <-
            readSingleGdbValue
              headGdb
              ("DNA2-" <> show num)
              ("x/1wx " <> showHex32 (myBaseAddr + 0x08))
          headDeviceDna1 <-
            readSingleGdbValue
              headGdb
              ("DNA2-" <> show num)
              ("x/1wx " <> showHex32 (myBaseAddr + 0x0C))
          headDeviceDna0 <-
            readSingleGdbValue
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
      ccGdbStarts = liftIO <$> L.zipWith (initGdb hitlDir "clock-control") ccPorts targets
    brackets ccGdbStarts (liftIO . snd) $ \initCCGdbsData -> do
      let ccGdbs = fst <$> initCCGdbsData
      liftIO $ putStrLn "Checking for MMIO access to SwCC CPUs over GDB..."
      gdbExitCodes0 <- mapM ccGdbCheck ccGdbs
      (gdbCount0, gdbExitCode0) <-
        L.foldl foldExitCodes (pure (0, ExitSuccess)) gdbExitCodes0
      liftIO
        $ putStrLn
          [i|CC GDB testing passed on #{gdbCount0} of #{L.length targets} targets|]
      liftIO $ mapM_ ((errorToException =<<) . Gdb.loadBinary) ccGdbs
      let muGdbStarts = liftIO <$> L.zipWith (initGdb hitlDir "management-unit") muPorts targets
      brackets muGdbStarts (liftIO . snd) $ \initMUGdbsData -> do
        let muGdbs = fst <$> initMUGdbsData
        liftIO $ putStrLn "Checking for MMIO access to MU CPUs over GDB..."
        gdbExitCodes1 <- mapM muGdbCheck muGdbs
        (gdbCount1, gdbExitCode1) <-
          L.foldl foldExitCodes (pure (0, ExitSuccess)) gdbExitCodes1
        liftIO
          $ putStrLn
            [i|MU GDB testing passed on #{gdbCount1} of #{L.length targets} targets|]
        liftIO $ mapM_ ((errorToException =<<) . Gdb.loadBinary) muGdbs

        let
          goDumpCcSamples = do
            _ <- dumpCcSamples hitlDir ccGdbs

            -- TODO: Move to separate CI step
            let ccSamplesPaths = [[i|#{hitlDir}/cc-samples-#{n}.bin|] | n <- [(0 :: Int) .. 7]]
            putStrLn "Rendering plots..."
            callProcess plotDataDumpPath ccSamplesPaths

        let picocomStarts = liftIO <$> L.zipWith (initPicocom hitlDir) targets [0 ..]
        brackets picocomStarts (liftIO . snd) $ \(L.map fst -> picocoms) -> do
          liftIO $ mapM_ Gdb.continue ccGdbs
          liftIO $ mapM_ Gdb.continue muGdbs
          liftIO
            $ tryWithTimeoutOn "Waiting for stable links" 60_000_000 goDumpCcSamples
            $ forConcurrently_ picocoms
            $ \pico ->
              waitForLine pico.stdoutHandle "[CC] All links stable"

          liftIO $ putStrLn "Getting UGNs for all targets"
          liftIO $ mapM_ Gdb.interrupt muGdbs
          ugnPairsTable <- zipWithM muGetUgns targets muGdbs
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
