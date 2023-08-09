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
        L.concatMap (\[a, b, c, d] -> [d, c, b, a]) $
        chunkFill 4 0 dat

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

    chunkFill :: Int -> a -> [a] -> [[a]]
    chunkFill _ _ [] = []
    chunkFill n fill xs =
      let (first0, rest) = L.splitAt n xs
          first1 = first0 <> L.replicate (n - L.length first0) fill
       in first1 : chunkFill n fill rest
