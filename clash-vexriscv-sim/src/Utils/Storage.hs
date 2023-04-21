-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
module Utils.Storage where

import Clash.Prelude

import qualified Data.List as L
import qualified Data.IntMap.Strict as I

import Clash.Signal.Internal (Signal((:-)))
import Protocols.Wishbone

storage ::
  forall dom.
  ( KnownDomain dom,
    HiddenClockResetEnable dom
  ) =>
  [BitVector 8] ->
  -- ^ contents
  Signal dom (WishboneM2S 32 4 (BitVector 32)) ->
  Signal dom (WishboneS2M (BitVector 32))
storage contents = mealy' go (I.fromAscList $ L.zip [0..] contents)
 where
  size = L.length contents

  -- Version of mealy that doesn't require NFDataX for the state.
  -- This is needed because IntMap (Word8) does not implement NFDataX
  mealy' fn st0 (i :- is) = o :- mealy' fn st1 is 
    where (st1, o) = fn st0 i

  go mem WishboneM2S{..}
    | not (busCycle && strobe)        = (mem, emptyWishboneS2M)
    | addr >= fromIntegral size       = (mem, emptyWishboneS2M { err = True })
    | not writeEnable      {- read -} =
        case readDataSel mem addr busSelect of
          Nothing -> (mem, emptyWishboneS2M { err = True })
          Just x -> (mem, (emptyWishboneS2M @(BitVector 32)) { acknowledge = True, readData = x })
    | otherwise           {- write -} =
        (writeDataSel mem addr busSelect writeData, emptyWishboneS2M { acknowledge = True })

readDataSel :: I.IntMap (BitVector 8) -> BitVector 32 -> BitVector 4 -> Maybe (BitVector 32)
readDataSel mem addr sel =
  case sel of
    $(bitPattern "0001") -> readByte (addr + 0)
    $(bitPattern "0010") -> readByte (addr + 1)
    $(bitPattern "0100") -> readByte (addr + 2)
    $(bitPattern "1000") -> readByte (addr + 3)
    $(bitPattern "0011") -> readWord (addr + 0)
    $(bitPattern "1100") -> readWord (addr + 2)
    $(bitPattern "1111") -> readDWord addr
    _                    -> Nothing
  
  where
    readByte addr' = resize @_ @8 @32 <$> I.lookup (fromIntegral addr') mem
    readWord addr' = do
      l <- readByte (addr' + 1)
      h <- readByte (addr' + 0)
      pure $ h `shiftL` 8 .|. l
    readDWord addr' = do
      l <- readWord (addr' + 2)
      h <- readWord (addr' + 0)
      pure $ h `shiftL` 16 .|. l

writeDataSel :: I.IntMap (BitVector 8) -> BitVector 32 -> BitVector 4 -> BitVector 32 -> I.IntMap (BitVector 8)
writeDataSel mem addr sel val =
  case sel of
    $(bitPattern "0001") ->
      I.insert (fromIntegral $ addr + 3) ll mem
    $(bitPattern "0010") ->
      I.insert (fromIntegral $ addr + 2) lh mem
    $(bitPattern "0100") ->
      I.insert (fromIntegral $ addr + 1) hl mem
    $(bitPattern "1000") ->
      I.insert (fromIntegral $ addr + 0) hh mem
    $(bitPattern "0011") ->
      I.insert (fromIntegral $ addr + 3) ll $
      I.insert (fromIntegral $ addr + 2) lh mem
    $(bitPattern "1100") ->
      I.insert (fromIntegral $ addr + 1) hl $
      I.insert (fromIntegral $ addr + 0) hh mem
    $(bitPattern "1111") ->
      I.insert (fromIntegral $ addr + 3) ll $
      I.insert (fromIntegral $ addr + 2) lh $
      I.insert (fromIntegral $ addr + 1) hl $
      I.insert (fromIntegral $ addr + 0) hh mem
    _ -> mem
  
  where
    (hh :: BitVector 8, hl :: BitVector 8, lh :: BitVector 8, ll :: BitVector 8) = unpack val