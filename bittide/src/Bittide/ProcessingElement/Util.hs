-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}
module Bittide.ProcessingElement.Util where

import Clash.Prelude hiding (Exp)

import Bittide.ProcessingElement.DeviceTreeCompiler
import Bittide.ProcessingElement.ReadElf
import Bittide.SharedTypes

import Clash.Explicit.BlockRam.File
import Data.Maybe
import Language.Haskell.TH
import Numeric (showHex)
import System.Exit

import qualified Data.List as L
import qualified Data.ByteString as BS
import qualified Data.IntMap as I
import Control.Monad (when)
import System.IO (stderr, hPutStrLn)

-- | Given the path to an elf file, the path to a device tree and a starting address
--  for the device tree. Return a 3 tuple containing:
--  (initial program counter, instruction memory blob, data memory blob)
memBlobsFromElf :: FilePath -> Maybe FilePath -> I.Key -> Q Exp
memBlobsFromElf elfPath deviceTreePath fdtAddr = do
  (iMemIntMap, dMemIntMap) <- runIO (getBytesMems elfPath deviceTreePath fdtAddr)
  let
    iResult = intMapToMemBlob iMemIntMap
    dResult = intMapToMemBlob dMemIntMap

  [|($iResult, $dResult)|]

-- | Given the path to an elf file, the path to a device tree and a starting address
--  for the device tree. Return a 3 tuple containing:
-- Return a 3 tuple containing (initial program counter, instruction memory blob, data memory blob)
getBytesMems :: FilePath -> Maybe FilePath -> I.Key -> IO (I.IntMap Byte, I.IntMap Byte)
getBytesMems elfPath deviceTreePath fdtAddr = do

  elfBytes <- BS.readFile elfPath
  let (entry, iMem, dMem0) = readElfFromMemory elfBytes

  when (entry /= 0x8000_0000) $ do
    hPutStrLn stderr $
      "Entry point of ELF file at " <> show elfPath <>
        " must be 0x80000000. Found 0x" <> showHex entry "" <> " instead"
    exitFailure

  -- add device tree as a memory mapped component
  deviceTree <- maybe (pure []) readDeviceTree deviceTreePath
  let
    deviceTreeMap = I.fromAscList (L.zip [fdtAddr ..] deviceTree)
    dMem1 = I.unionWithKey (\k _ _ -> error $
      "Bittide.ProcessingElement.Util: Overlapping element in data memory and device tree at address 0x"
      <> showHex k "") dMem0 deviceTreeMap

  putStrLn $ "elf file: " <> elfPath <>
          "\ndevice tree: " <> fromMaybe "None" deviceTreePath <>
          "\ndevice tree starting address: " <> show fdtAddr <>
          "\n device tree size: " <> showHex (L.length deviceTree) ""

  pure (iMem, if isJust deviceTreePath then dMem1 else dMem0)

-- | Given an IntMap, return a 3 tuple containing:
-- (starting address, size, memBlob)
intMapToMemBlob :: I.IntMap Byte -> Q Exp
intMapToMemBlob intMap = do
  let
    (startAddr, size, mapAsList) = extractIntMapData intMap
    memBlob = memBlobTH Nothing mapAsList
  [| (startAddr, size :: Integer, $memBlob) |]

-- | Write a list of `Byte`s to a file to be used with `blockRamFile`.
writeByteListToFile :: FilePath -> [Byte] -> IO ()
writeByteListToFile filePath byteList = do
  let fileContent = memFile Nothing byteList
  writeFile filePath fileContent

-- | Given an IntMap, return the starting address, size and content as @[Bytes 4]@
extractIntMapData ::
  -- | IntMap
  I.IntMap (BitVector 8) ->
  -- |
  -- 1. Starting address
  -- 2. Size
  -- 3. List of words
  (BitVector 32, Int, [Bytes 4])
extractIntMapData dataMap = (resize . bitCoerce $ startAddr, size, toBytes4 content)
 where
  ordList = I.toAscList dataMap
  startAddr = fst $ L.head ordList
  size = I.size dataMap
  content =
    snd (L.head ordList) : flattenContent startAddr (L.tail ordList)

  flattenContent _ [] = []
  flattenContent prevAddr ((nextAddr, val):vals) =
    let
      n = nextAddr - prevAddr - 1
      padding = L.replicate n 0
    in padding L.++ (val : flattenContent nextAddr vals)

  toBytes4 :: [Bytes 1] -> [Bytes 4]
  toBytes4 [] = []
  toBytes4 [!a] = [bitCoerce (a, 0 :: Bytes 3)]
  toBytes4 [!a, !b] = [bitCoerce (a, b, 0 :: Bytes 2)]
  toBytes4 [!a, !b, !c] = [bitCoerce (a, b, c, 0 :: Bytes 1)]
  toBytes4 ((!a):(!b):(!c):(!d):rest) = bitCoerce (a, b, c, d) : toBytes4 rest

-- | Given the filepath to a device tree, return the divce tree as list of `Byte`.
readDeviceTree :: FilePath -> IO [Byte]
readDeviceTree deviceTreePath = do
    compileRes <- compileDeviceTreeSource deviceTreePath

    deviceTreeRaw <- maybe exitFailure pure compileRes

    let
      padding = L.replicate (4 - (BS.length deviceTreeRaw `mod` 4)) 0

    pure (fmap pack . BS.unpack $ deviceTreeRaw <> BS.pack padding)
