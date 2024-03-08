-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NumericUnderscores #-}
module Utils.ProgramLoad where

import Clash.Prelude

import Protocols.Wishbone

import qualified Data.ByteString as BS
import qualified Data.IntMap as I
import qualified Data.List as L

import Control.Exception (assert)

import Utils.ReadElf
import Utils.Storage

type DMemory dom =
  Signal dom (WishboneM2S 32 4 (BitVector 32)) ->
  Signal dom (WishboneM2S 32 4 (BitVector 32)) ->
  (Signal dom (WishboneS2M (BitVector 32)), Signal dom (WishboneS2M (BitVector 32)))

loadProgramDmem :: (HiddenClockResetEnable dom) => FilePath -> IO (DMemory dom, DMemory dom)
loadProgramDmem path = do
  elfBytes <- BS.readFile path
  let (entry, iMem, dMem) = readElfFromMemory elfBytes

  assert (entry == 0x2000_0000) (pure ())

  let
      endianSwap dat =
        L.concatMap (\(a, b, c, d) -> [d, c, b, a]) $
        chunkFill4 0 dat

      -- endian swap instructions
      iMemContents = endianSwap $
        content iMem <> [0, 0, 0, 0, 0, 0, 0, 0]
      dMemContents = endianSwap $
        content dMem <> [0, 0, 0, 0, 0, 0, 0, 0]


  let instrMem = dualPortStorage iMemContents
      dataMem = dualPortStorage dMemContents

  pure (instrMem, dataMem)
  where
    content :: BinaryData -> [BitVector 8]
    content bin = L.map snd $ I.toAscList bin

    chunkFill4 :: a -> [a] -> [(a, a, a, a)]
    chunkFill4 fill = \case
      [] -> []
      [a] -> [(a, fill, fill, fill)]
      [a, b] -> [(a, b, fill, fill)]
      [a, b, c] -> [(a, b, c, fill)]
      (a:b:c:d:rest) -> (a, b, c, d) : chunkFill4 fill rest




type Memory dom =
  ( Signal dom (WishboneM2S 32 4 (BitVector 32)) ->
    Signal dom (WishboneS2M (BitVector 32))
  )

loadProgram :: (HiddenClockResetEnable dom) => FilePath -> IO (Memory dom, Memory dom)
loadProgram path = do
  elfBytes <- BS.readFile path
  let (entry, iMem, dMem) = readElfFromMemory elfBytes

  assert (entry == 0x2000_0000) (pure ())

  let
      endianSwap dat =
        L.concatMap (\(a, b, c, d) -> [d, c, b, a]) $
        chunkFill4 0 dat

      -- endian swap instructions
      iMemContents = endianSwap $
        content iMem <> [0, 0, 0, 0, 0, 0, 0, 0]
      dMemContents = endianSwap $
        content dMem <> [0, 0, 0, 0, 0, 0, 0, 0]


  let instrMem = storage iMemContents
      dataMem = storage dMemContents

  pure (instrMem, dataMem)
  where
    content :: BinaryData -> [BitVector 8]
    content bin = L.map snd $ I.toAscList bin

    chunkFill4 :: a -> [a] -> [(a, a, a, a)]
    chunkFill4 fill = \case
      [] -> []
      [a] -> [(a, fill, fill, fill)]
      [a, b] -> [(a, b, fill, fill)]
      [a, b, c] -> [(a, b, c, fill)]
      (a:b:c:d:rest) -> (a, b, c, d) : chunkFill4 fill rest
