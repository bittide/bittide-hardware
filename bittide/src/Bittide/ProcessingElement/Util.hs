-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.ProcessingElement.Util where

import Clash.Prelude hiding (Exp)
import Clash.Sized.Vector (unsafeFromList)

import Bittide.ProcessingElement.DeviceTreeCompiler
import Bittide.ProcessingElement.ReadElf
import Bittide.SharedTypes

import Control.Monad (when)
import Data.Maybe
import GHC.Stack
import Numeric (showHex)
import System.Exit
import System.IO (hPutStrLn, stderr)

import qualified Data.ByteString as BS
import qualified Data.IntMap as I
import qualified Data.List as L

{- | Given a `Maybe Int` and a list, if the `Maybe Int` is `Just s`, pad the list to
size `s` with the given element. If the length of the list is greater than `s`,
throw an error. If the `Maybe Int` is `Nothing`, return the list as is.
-}
padToSize :: (HasCallStack) => String -> Maybe Int -> a -> [a] -> [a]
padToSize _ Nothing _ l = l
padToSize name (Just s) a xs
  | s >= l = xs <> L.replicate (s - l) a
  | otherwise =
      error
        $ "Bittide.ProcessingElement.Util: "
        <> name
        <> " with length "
        <> show l
        <> " is longer than the specified size "
        <> show s
 where
  l = L.length xs

-- | Given the path to an elf file, return a tuple containing @(instruction memory, data memory)@.
vecsFromElf ::
  forall nInstrWords nDataWords.
  (HasCallStack, KnownNat nDataWords, KnownNat nInstrWords) =>
  -- | Source file, assumed to be Little Endian.
  FilePath ->
  -- | Optional tuple of starting address and filepath to a device tree.
  Maybe (I.Key, FilePath) ->
  -- | (instruction memBlob, data memBlob)
  IO
    ( Vec nInstrWords (BitVector 32)
    , Vec nDataWords (BitVector 32)
    )
vecsFromElf elfPath maybeDeviceTree = do
  (iMemIntMap, dMemIntMap) <- getBytesMems elfPath maybeDeviceTree
  let
    (_iStartAddr, _, iList) = extractIntMapData iMemIntMap
    (_dStartAddr, _, dList) = extractIntMapData dMemIntMap
    iListPadded = padToSize "Instruction memory" (Just (natToNum @nInstrWords)) 0 iList
    dListPadded = padToSize "Data memory" (Just (natToNum @nInstrWords)) 0 dList

  pure (unsafeFromList iListPadded, unsafeFromList dListPadded)

-- | Read data memory from an ELF file.
vecFromElfData ::
  forall nWords.
  (HasCallStack, KnownNat nWords) =>
  -- | Source file, assumed to be Little Endian.
  FilePath ->
  -- | Data memory
  IO (Vec nWords (BitVector 32))
vecFromElfData elfPath = do
  (_iMemIntMap, dMemIntMap) <- getBytesMems elfPath Nothing
  let
    (_dStartAddr, _, dList) = extractIntMapData dMemIntMap
    dListPadded = padToSize "Data memory" (Just (natToNum @nWords)) 0 dList

  pure (unsafeFromList dListPadded)

-- | Read instruction memory from an ELF file.
vecFromElfInstr ::
  forall nWords.
  (HasCallStack, KnownNat nWords) =>
  -- | Source file, assumed to be Little Endian.
  FilePath ->
  -- | Instruction memory
  IO (Vec nWords (BitVector 32))
vecFromElfInstr elfPath = do
  (iMemIntMap, _dMemIntMap) <- getBytesMems elfPath Nothing
  let
    (_iStartAddr, _, iList) = extractIntMapData iMemIntMap
    iListPadded = padToSize "Instruction memory" (Just (natToNum @nWords)) 0 iList

  pure (unsafeFromList iListPadded)

{- | Given the path to an elf file, the path to a device tree and a starting address
 for the device tree. Return a 3 tuple containing:
Return a 3 tuple containing (initial program counter, instruction memory blob, data memory blob)
-}
getBytesMems :: FilePath -> Maybe (I.Key, FilePath) -> IO (I.IntMap Byte, I.IntMap Byte)
getBytesMems elfPath maybeDeviceTree = do
  elfBytes <- BS.readFile elfPath
  let (entry, iMem, dMem0) = readElfFromMemory elfBytes

  when (entry /= 0x8000_0000) $ do
    hPutStrLn stderr
      $ "Entry point of ELF file at "
      <> show elfPath
      <> " must be 0x80000000. Found 0x"
      <> showHex entry ""
      <> " instead"
    exitFailure

  -- add device tree as a memory mapped component
  deviceTree <- maybe (pure []) (readDeviceTree . snd) maybeDeviceTree
  let
    fdtAddr = maybe 0 fst maybeDeviceTree
    deviceTreeMap = I.fromList (L.zip [fdtAddr ..] deviceTree)
    dMem1 =
      I.unionWithKey
        ( \k _ _ ->
            error
              $ "Bittide.ProcessingElement.Util: Overlapping element in data memory and device tree at address 0x"
              <> showHex k ""
        )
        dMem0
        deviceTreeMap

  pure (iMem, if isJust maybeDeviceTree then dMem1 else dMem0)

{- | Given an IntMap, return the starting address, size and content as @[Bytes 4]@. Errors
if the IntMap is empty.
-}
extractIntMapData ::
  (HasCallStack) =>
  -- | IntMap
  I.IntMap (BitVector 8) ->
  {- |
  1. Starting address
  2. Size
  3. List of words
  -}
  (BitVector 32, Int, [Bytes 4])
extractIntMapData dataMap =
  (resize . bitCoerce $ startAddr, size, toWords content)
 where
  (ordListHead, ordListTail) = case I.toAscList dataMap of
    [] -> error "extractIntMapData: IntMap is empty"
    (x : xs) -> (x, xs)

  startAddr = fst ordListHead
  size = I.size dataMap
  content =
    snd ordListHead : flattenContent startAddr ordListTail

  flattenContent _ [] = []
  flattenContent prevAddr ((nextAddr, val) : vals) =
    let
      n = nextAddr - prevAddr - 1
      padding = L.replicate n 0
     in
      padding L.++ (val : flattenContent nextAddr vals)

  toWords :: [Bytes 1] -> [Bytes 4]
  toWords [] = []
  toWords [a] = toWords [a, 0, 0, 0]
  toWords [a, b] = toWords [a, b, 0, 0]
  toWords [a, b, c] = toWords [a, b, c, 0]
  toWords (a : b : c : d : rest) =
    -- Note the way we pack: this might seem counterintuitive, but it makes sure that the
    -- lower addresses are packed into the lower bits.
    let
      !packed = pack (d, c, b, a)
     in
      packed : toWords rest

-- | Given the filepath to a device tree, return the divce tree as list of `Byte`.
readDeviceTree :: FilePath -> IO [Byte]
readDeviceTree deviceTreePath = do
  compileRes <- compileDeviceTreeSource deviceTreePath

  deviceTreeRaw <- maybe exitFailure pure compileRes

  let
    padding = L.replicate (4 - (BS.length deviceTreeRaw `mod` 4)) 0

  pure (fmap pack . BS.unpack $ deviceTreeRaw <> BS.pack padding)
