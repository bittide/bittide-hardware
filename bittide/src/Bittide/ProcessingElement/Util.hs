-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}
module Bittide.ProcessingElement.Util where

import Clash.Prelude hiding (Exp)

import Bittide.ProcessingElement.DeviceTreeCompiler
import Bittide.ProcessingElement.ReadElf
import Bittide.SharedTypes

import Control.Monad (when)
import Data.Maybe
import GHC.Stack
import Language.Haskell.TH
import Numeric (showHex)
import System.Exit
import System.IO (stderr, hPutStrLn)

import qualified Data.ByteString as BS
import qualified Data.IntMap as I
import qualified Data.List as L

-- | Given a `Maybe Int` and a list, if the `Maybe Int` is `Just s`, pad the list to
-- size `s` with the given element. If the length of the list is greater than `s`,
-- throw an error. If the `Maybe Int` is `Nothing`, return the list as is.
padToSize :: HasCallStack => String -> Maybe Int -> a -> [a] -> [a]
padToSize _ Nothing _ l = l
padToSize name (Just s) a xs
  | s >= l = xs <> L.replicate (s - l) a
  | otherwise = error $ "Bittide.ProcessingElement.Util: " <> name <> " with length " <>
    show l <> " is longer than the specified size " <> show s
  where
    l = L.length xs


-- | Given the path to an elf file, the path to a device tree and a starting address
--  for the device tree. Return a 3 tuple containing:
--  (initial program counter, instruction memory blob, data memory blob)
memBlobsFromElf ::
  HasCallStack =>
  -- | How the words should be ordered in the memBlob
  ByteOrder ->
  -- | Optional size in bytes to which we should pad the instruction memBlob and data memBlob.
  -- Rounds up to the nearest word size.
  (Maybe Int, Maybe Int) ->
  -- | Source file, assumed to be Little Endian.
  FilePath ->
  -- | Optional tuple of starting address and filepath to a device tree.
  Maybe (I.Key, FilePath) ->
  -- | (instruction memBlob, data memBlob)
   Q Exp
memBlobsFromElf byteOrder (iSize, dSize) elfPath maybeDeviceTree = do
  (iMemIntMap, dMemIntMap) <- runIO (getBytesMems elfPath maybeDeviceTree)
  let
    (_iStartAddr, _, iList) = extractIntMapData byteOrder iMemIntMap
    (_dStartAddr, _, dList) = extractIntMapData byteOrder dMemIntMap
    iListPadded = padToSize "Instruction memory" (fmap ((`div` 4) . (+3)) iSize) 0 iList
    dListPadded = padToSize "Data memory" (fmap ((`div` 4) . (+3)) dSize) 0 dList
    iBlob = memBlobTH Nothing iListPadded
    dBlob = memBlobTH Nothing dListPadded

  [|($iBlob, $dBlob)|]

-- | Given the path to an elf file, the path to a device tree and a starting address
--  for the device tree. Return a 3 tuple containing:
-- Return a 3 tuple containing (initial program counter, instruction memory blob, data memory blob)
getBytesMems :: FilePath -> Maybe (I.Key, FilePath) -> IO (I.IntMap Byte, I.IntMap Byte)
getBytesMems elfPath maybeDeviceTree = do

  elfBytes <- BS.readFile elfPath
  let (entry, iMem, dMem0) = readElfFromMemory elfBytes

  when (entry /= 0x8000_0000) $ do
    hPutStrLn stderr $
      "Entry point of ELF file at " <> show elfPath <>
        " must be 0x80000000. Found 0x" <> showHex entry "" <> " instead"
    exitFailure

  -- add device tree as a memory mapped component
  deviceTree <- maybe (pure []) (readDeviceTree . snd) maybeDeviceTree
  let
    fdtAddr = maybe 0 fst maybeDeviceTree
    deviceTreeMap = I.fromList (L.zip [fdtAddr ..] deviceTree)
    dMem1 = I.unionWithKey (\k _ _ -> error $
      "Bittide.ProcessingElement.Util: Overlapping element in data memory and device tree at address 0x"
      <> showHex k "") dMem0 deviceTreeMap

  pure (iMem, if isJust maybeDeviceTree then dMem1 else dMem0)

-- | Given an IntMap, return the starting address, size and content as @[Bytes 4]@
extractIntMapData ::
  ByteOrder ->
  -- | IntMap
  I.IntMap (BitVector 8) ->
  -- |
  -- 1. Starting address
  -- 2. Size
  -- 3. List of words
  (BitVector 32, Int, [Bytes 4])
extractIntMapData byteOrder dataMap = (resize . bitCoerce $ startAddr, size, combineFunction content)
 where
  combineFunction
    | LittleEndian <- byteOrder = toWordsLinear
    | BigEndian    <- byteOrder = toWordsSwapped

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

  toWordsLinear :: [Bytes 1] -> [Bytes 4]
  toWordsLinear [] = []
  toWordsLinear [!a] = [bitCoerce (a, 0 :: Bytes 3)]
  toWordsLinear [!a, !b] = [bitCoerce (a, b, 0 :: Bytes 2)]
  toWordsLinear [!a, !b, !c] = [bitCoerce (a, b, c, 0 :: Bytes 1)]
  toWordsLinear ((!a):(!b):(!c):(!d):rest) = bitCoerce (a, b, c, d) : toWordsLinear rest

  toWordsSwapped :: [Bytes 1] -> [Bytes 4]
  toWordsSwapped [] = []
  toWordsSwapped [!a] = [bitCoerce (a, 0 :: Bytes 3)]
  toWordsSwapped [!a, !b] = [bitCoerce (b, a, 0 :: Bytes 2)]
  toWordsSwapped [!a, !b, !c] = [bitCoerce (c, b, a, 0 :: Bytes 1)]
  toWordsSwapped ((!a):(!b):(!c):(!d):rest) = bitCoerce (d, c, b, a) : toWordsSwapped rest

-- | Given the filepath to a device tree, return the divce tree as list of `Byte`.
readDeviceTree :: FilePath -> IO [Byte]
readDeviceTree deviceTreePath = do
    compileRes <- compileDeviceTreeSource deviceTreePath

    deviceTreeRaw <- maybe exitFailure pure compileRes

    let
      padding = L.replicate (4 - (BS.length deviceTreeRaw `mod` 4)) 0

    pure (fmap pack . BS.unpack $ deviceTreeRaw <> BS.pack padding)
