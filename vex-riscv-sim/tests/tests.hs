-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NumericUnderscores #-}

import Prelude
import Clash.Prelude hiding (not, (&&))

import VexRiscv
import Utils.ReadElf

import Bittide.DoubleBufferedRam
import Bittide.SharedTypes hiding (delayControls)
import Bittide.Wishbone (singleMasterInterconnect')
import Protocols.Wishbone

import Clash.Prelude.BlockRam.File
import Clash.Signal.Internal (Signal((:-)))

import qualified Data.ByteString as BS
import qualified Data.IntMap as I
import qualified Data.List as L
import qualified GHC.TypeNats as TN

import Test.Tasty
import GHC.Stack
import GHC.Base (assert, when)

import System.Directory (removeFile, listDirectory, doesFileExist, copyFile)
import System.IO
import System.FilePath
import Control.Monad (forM)
import Data.Maybe (catMaybes)
import Test.Tasty.HUnit (testCase, Assertion, (@?=))
import System.IO.Temp (withSystemTempFile)
import Utils.Print (getPrintContents)
import System.Exit (exitFailure)


emptyInput :: Input
emptyInput =
  Input
    { timerInterrupt = low
    , externalInterrupt = low
    , softwareInterrupt = low
    , iBusWbS2M = (emptyWishboneS2M @(BitVector 32)) {readData = 0}
    , dBusWbS2M = (emptyWishboneS2M @(BitVector 32)) {readData = 0}
    }

type Memory dom = (BitVector 32, Signal dom (WishboneM2S 32 4 (BitVector 32))
  -> Signal dom (WishboneS2M (BitVector 32)))

-- When passing S2M values from Haskell to VexRiscv over the FFI, undefined
-- bits/values cause errors when forcing their evaluation to something that can
-- be passed through the FFI.
--
-- This function makes sure the Wishbone S2M values are free from undefined bits.
makeDefined :: WishboneS2M (BitVector 32) -> WishboneS2M (BitVector 32)
makeDefined wb = wb { readData = defaultX 0 (readData wb) }

defaultX :: (NFDataX a) => a -> a -> a
defaultX dflt val
  | hasUndefined val = dflt
  | otherwise        = val

cpu ::
  (HasCallStack, HiddenClockResetEnable dom) =>
  Memory dom ->
  Memory dom ->
  ( Signal dom Output
    -- writes
  , Signal dom (Maybe (BitVector 32, BitVector 32))
    -- iBus responses
  , Signal dom (WishboneS2M (BitVector 32))
    -- dBus responses
  , Signal dom (WishboneS2M (BitVector 32))
  )
cpu (iMemStart, iMem) (_dMemStart, dMem) = (output, writes, iWb, dS2M)
  where
    output = vexRiscv (emptyInput :- input)
    dM2S = dBusWbM2S <$> output
    iWb = iMem (mapAddr (\x -> x - iMemStart) . unBusAddr . iBusWbM2S <$> output)

    (_, dummy) = dummyWb 0x0000_0000
    -- both dummyS2M and dMemS2M use 32 bit, but the M2S signals are 30bit
    dummyS2M = dummy (mapAddr @30 @32 resize <$> dummyM2S)
    dMemS2M = dMem (mapAddr @30 @32 resize <$> dMemM2S)

    dSlaveS2Ms = bundle (dMemS2M :> dummyS2M :> Nil)

    (dS2M, unbundle -> (dMemM2S :> dummyM2S :> Nil)) =
      singleMasterInterconnect'
        -- 1 bit prefixes (31bit -> 30 bit after prefix removal)
        (0x1 :> 0x0 :> Nil)
        -- going from 0b0xyz to 0bxyz (32bit to 31 bit)
        (mapAddr @32 @31 resize . unBusAddr <$> dM2S)
        dSlaveS2Ms

    input =
      ( \iBus dBus ->
          Input
            { timerInterrupt = low
            , externalInterrupt = low
            , softwareInterrupt = low
            , iBusWbS2M = makeDefined iBus
            , dBusWbS2M = makeDefined dBus
            }
      )
        <$> iWb
        <*> dS2M

    unBusAddr = mapAddr ((`shiftL` 2) . extend @_ @_ @2)

    writes = mux
      ( (busCycle <$> dM2S) .&&. (strobe <$> dM2S) .&&. (writeEnable <$> dM2S)
          .&&. (acknowledge <$> dS2M) )
      (do
        dM2S' <- dM2S
        pure $ Just (extend (addr dM2S') `shiftL` 2, writeData dM2S')
        )
      (pure Nothing)

mapAddr :: (BitVector aw1 -> BitVector aw2) -> WishboneM2S aw1 selWidth a -> WishboneM2S aw2 selWidth a
mapAddr f wb = wb { addr = f (addr wb) }

-- | Wishbone circuit that always acknowledges every request
--
-- Used for the character device. The character device address gets mapped to this
-- component because if it were to be routed to the data memory (where this address is
-- not in the valid space) it would return ERR and would halt execution.
dummyWb :: (HiddenClockResetEnable dom) => BitVector 32 -> Memory dom
dummyWb address = (address, \m2s -> delayControls m2s (reply <$> m2s))
  where
    reply WishboneM2S{..} =
      (emptyWishboneS2M @(BitVector 32)) { acknowledge = acknowledge, readData = 0 }
     where
      acknowledge = busCycle && strobe

    -- | Delays the output controls to align them with the actual read / write timing.
    delayControls ::
      (HiddenClockResetEnable dom, NFDataX a) =>
      Signal dom (WishboneM2S addressWidth selWidth a) -> -- current M2S signal
      Signal dom (WishboneS2M a) ->
      Signal dom (WishboneS2M a)
    delayControls m2s s2m0 = mux inCycle s2m1 (pure emptyWishboneS2M)
     where
      inCycle = (busCycle <$> m2s) .&&. (strobe <$> m2s)

      -- It takes a single cycle to lookup elements in a block ram. We can therfore
      -- only process a request every other clock cycle.
      ack = (acknowledge <$> s2m0) .&&. (not <$> delayedAck) .&&. inCycle
      err1 = (err <$> s2m0) .&&. inCycle
      delayedAck = register False ack
      delayedErr1 = register False err1
      s2m1 = (\wb newAck newErr-> wb{acknowledge = newAck, err = newErr})
        <$> s2m0 <*> delayedAck <*> delayedErr1

loadProgram :: (HiddenClockResetEnable dom) => FilePath -> IO (IO (), Memory dom, Memory dom)
loadProgram path = do
  elfBytes <- BS.readFile path
  let (entry, iMem, dMem) = readElfFromMemory elfBytes

  assert (entry == 0x8000_0000) (pure ())

  (iPath, iHandle) <- openTempFile "/tmp" "imem.blob"

  (d0Path, d0Handle) <- openTempFile "/tmp" "dmem.0.blob"
  (d1Path, d1Handle) <- openTempFile "/tmp" "dmem.1.blob"
  (d2Path, d2Handle) <- openTempFile "/tmp" "dmem.2.blob"
  (d3Path, d3Handle) <- openTempFile "/tmp" "dmem.3.blob"

  let removeFiles = mapM_ removeFile [iPath, d0Path, d1Path, d2Path, d3Path]

  let
    -- endian swap instructions
    iMemBS = memFile Nothing $
        L.map (\[a, b, c, d] -> bitCoerce (d, c, b, a) :: BitVector 32) $
        chunkFill 4 0 (content iMem)

    (dL0, dL1, dL2, dL3) = split4 $ content dMem
    dMem0BS = memFile Nothing dL0
    dMem1BS = memFile Nothing dL1
    dMem2BS = memFile Nothing dL2
    dMem3BS = memFile Nothing dL3

    iMemStart = startAddr iMem
    dMemStart = startAddr dMem

    iMemSize = I.size iMem `divRU` 4
    dMemSize = I.size dMem `divRU` 4

    dContentVec = d0Path :> d1Path :> d2Path :> d3Path :> Nil

  assert (dMemStart == 0x4000_0000) (pure ())

  -- write data to files
  hPutStr iHandle iMemBS
  -- endian swap data
  hPutStr d0Handle dMem3BS
  hPutStr d1Handle dMem2BS
  hPutStr d2Handle dMem1BS
  hPutStr d3Handle dMem0BS

  -- close files
  hClose iHandle
  hClose d0Handle
  hClose d1Handle
  hClose d2Handle
  hClose d3Handle

  let
    instrMem = case TN.someNatVal (toEnum iMemSize) of
      SomeNat (snatProxy -> depth) ->
        case compareSNat depth d1 of
          SNatLE -> error "should not happen"
          SNatGT ->
            let
              initContent = helper depth $ Reloadable $ File iPath
            in (iMemStart, wbStorage' initContent)

    dataMem = case TN.someNatVal (toEnum dMemSize) of
      SomeNat (snatProxy -> depth) ->
        case compareSNat depth d1 of
          SNatLE -> error "should not happen"
          SNatGT ->
            let initContent = helper depth $ NonReloadable $ FileVec dContentVec
            in (dMemStart, wbStorage' initContent)


  pure (removeFiles, instrMem, dataMem)
 where

  helper ::
    SNat depth ->
    InitialContent depth (BitVector 32) ->
    InitialContent depth (BitVector 32)
  helper SNat cont = cont

  startAddr :: BinaryData -> BitVector 32
  startAddr bin = resize . bitCoerce $ fst . L.head $ I.toAscList bin

  content :: BinaryData -> [BitVector 8]
  content bin = L.map snd $ I.toAscList bin

  split4 :: [BitVector 8] -> ([BitVector 8], [BitVector 8], [BitVector 8], [BitVector 8])
  split4 xs = L.unzip4 $ L.map (\[a, b, c, d] -> (a, b, c, d)) $ chunkFill 4 0 xs

  chunkFill :: Int -> a -> [a] -> [[a]]
  chunkFill _ _    [] = []
  chunkFill n fill xs =
    let (first0, rest) = L.splitAt n xs
        first1 = first0 <> L.replicate (n - L.length first0) fill
    in first1 : chunkFill n fill rest


runProgramExpect
  :: (FilePath -> IO ()) -- ^ action to copy ELF file
  -> Int -- ^ number of cycles to simulate
  -> BS.ByteString -- ^ expected output
  -> Assertion
runProgramExpect act n expected = withSystemTempFile "ELF" $ \fp _ -> do
  act fp
  (removeFiles, iMem, dMem) <- withClockResetEnable @System clockGen (resetGenN (SNat @2)) enableGen $ loadProgram fp

  let _all@(unbundle -> (_circuit, writes, _iBus, _dBus)) = withClockResetEnable @System clockGen (resetGenN (SNat @2)) enableGen $
        bundle (cpu iMem dMem)


  let output = getPrintContents (BS.length expected) 0x2000_0000 $
        L.map bitCoerce $ sampleN_lazy n writes

  output @?= expected
  removeFiles

findTests ::
  FilePath ->
  FilePath ->
  IO [(String, FilePath, FilePath)]
  -- test name  bin path  expected-path
findTests srcDir binDir = do
  srcFiles <- listDirectory srcDir

  let expectFiles = L.filter (\p -> takeExtension p == ".expected") srcFiles
      binaryPaths = L.map (\p -> binDir </> takeBaseName p) expectFiles

  paths <- forM (L.zip binaryPaths expectFiles) $ \(binPath, expectPath) -> do
    exists <- doesFileExist binPath
    if exists
      then
        pure $ Just (takeBaseName binPath, binPath, srcDir </> expectPath)
      else do
        hPutStrLn stderr $
          "No binary file found for test program " <> takeBaseName binPath
            <> " (expected path " <> binPath <> ")"
        pure Nothing

  pure $ catMaybes paths

sourceDir, releaseBinDir, debugBinDir :: FilePath
sourceDir = "vex-riscv-sim/vex-test-programs/src/bin"
releaseBinDir = "target/riscv32imc-unknown-none-elf/release"
debugBinDir = "target/riscv32imc-unknown-none-elf/debug"


releaseCycles, debugCycles :: Int
releaseCycles = 1_000_000 -- 1 million cycles
debugCycles = 1_000_000 -- 10 million cycles

runTest
  :: String -- ^ name of the test
  -> String -- ^ mode of the test (debug / release)
  -> Int -- ^ Cycles to simulate
  -> FilePath -- ^ path to the binary
  -> FilePath -- ^ path to the expected output file
  -> TestTree
runTest name mode n elfPath expectPath =
  testCase ("Integration test `" <> name <> "` (" <> mode <> ")") $ do
    expected <- BS.readFile expectPath
    let act = copyFile elfPath

    runProgramExpect act n expected
    pure ()

main :: IO ()
main = do
  debugTests <- findTests sourceDir debugBinDir
  releaseTests <- findTests sourceDir releaseBinDir

  when (L.null debugTests) $ do
    hPutStrLn stderr "No debug tests found! Was `cargo build` run?"
    exitFailure

  when (L.null releaseTests) $ do
    hPutStrLn stderr "No release tests found! Was `cargo build --release` run?"
    exitFailure

  let debugTestCases = flip L.map debugTests $ \(name, binPath, expectPath) ->
        runTest name "debug" debugCycles binPath expectPath

  let releaseTestCases = flip L.map releaseTests $ \(name, binPath, expectPath) ->
        runTest name "release" releaseCycles binPath expectPath

  let tests = testGroup "VexRiscv Tests" [
          testGroup "Debug builds" debugTestCases,
          testGroup "Release builds" releaseTestCases
        ]

  defaultMain tests
