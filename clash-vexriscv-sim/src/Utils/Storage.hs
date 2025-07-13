-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
-- it doesn't like lazy matching on `Signal`s it seems?
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Utils.Storage (
  storage,
  dualPortStorage,
) where

import Clash.Prelude
import Clash.Signal.Internal (Signal ((:-)))
import Data.Either (isLeft)
import GHC.Stack (HasCallStack)
import Protocols.Wishbone

import qualified Data.IntMap.Strict as I
import qualified Data.List as L

newtype MappedMemory = MappedMemory (I.IntMap (BitVector 8))

unMappedMemory :: MappedMemory -> I.IntMap (BitVector 8)
unMappedMemory (MappedMemory x) = x

instance NFDataX MappedMemory where
  -- Not a product type, so no spine
  deepErrorX = errorX

  -- Keys are 'Int's and evaluated to WHNF because this is a strict map. For 'Int's,
  -- WHNF ~ NF, so we only need to check the values.
  hasUndefined m =
    isLeft (isX (unMappedMemory m))
      || any hasUndefined (I.elems $ unMappedMemory m)

  -- Not a product type, so no spine
  ensureSpine = id

  -- This is a strict map, so we don't need to do anything. Note that WHNF ~ NF for
  -- 'BitVector'.
  rnfX x = seq x ()

dualPortStorage ::
  forall dom.
  ( KnownDomain dom
  , HiddenClockResetEnable dom
  ) =>
  -- | contents
  [BitVector 8] ->
  -- | in A
  Signal dom (WishboneM2S 32 4 (BitVector 32)) ->
  -- | in B
  Signal dom (WishboneM2S 32 4 (BitVector 32)) ->
  ( Signal dom (WishboneS2M (BitVector 32))
  , -- \^ out A
    Signal dom (WishboneS2M (BitVector 32))
  )
-- \^ out B

dualPortStorage contents portA portB = (aReply, bReply)
 where
  actualResult = storage contents inSignal

  (_port, inSignal, aReply, bReply) = unbundle $ go A portA portB actualResult

  go !currentPort (a :- inA) (b :- inB) ~(res :- actualResult')
    -- neither active, just say A is current, do nothing
    | not aActive && not bActive =
        (A, a, res, emptyWishboneS2M) :- (res `seq` next)
    -- A current, A active -> do A
    | currentPort == A && aActive =
        (A, a, res, emptyWishboneS2M) :- (res `seq` next)
    -- current A, A not active but B is, do B and switch to B
    | currentPort == A && not aActive && bActive =
        (B, b, emptyWishboneS2M, res) :- (res `seq` next)
    -- current B, B active -> do B
    | currentPort == B && bActive =
        (B, b, emptyWishboneS2M, res) :- (res `seq` next)
    -- current B, B not active, but A is, do A and switch to A
    | currentPort == B && not bActive && aActive =
        (A, a, res, emptyWishboneS2M) :- (res `seq` next)
   where
    aActive = strobe a && busCycle a
    bActive = strobe b && busCycle b

    nextPort = case (currentPort, aActive, bActive) of
      (_, False, False) -> A
      (A, False, True) -> B
      (A, True, _) -> A
      (B, _, True) -> B
      (B, True, False) -> A

    next = go nextPort inA inB actualResult'

data AorB = A | B deriving (Generic, NFDataX, Eq)

storage ::
  forall dom.
  ( KnownDomain dom
  , HiddenClockResetEnable dom
  ) =>
  -- | contents
  [BitVector 8] ->
  Signal dom (WishboneM2S 32 4 (BitVector 32)) ->
  Signal dom (WishboneS2M (BitVector 32))
storage contents = mealy go (MappedMemory $ I.fromAscList $ L.zip [0 ..] contents)
 where
  size = L.length contents

  go (MappedMemory !mem) WishboneM2S{..}
    | not (busCycle && strobe) = (MappedMemory mem, emptyWishboneS2M)
    | addr >= fromIntegral size =
        (MappedMemory mem, emptyWishboneS2M{err = True})
    | not writeEnable {- read -} =
        case readDataSel mem addr busSelect of
          Nothing -> (MappedMemory mem, emptyWishboneS2M{err = True})
          Just x -> (MappedMemory mem, (emptyWishboneS2M @(BitVector 32)){acknowledge = True, readData = x})
    | otherwise {- write -} =
        ( MappedMemory (writeDataSel mem addr busSelect writeData)
        , emptyWishboneS2M{acknowledge = True}
        )

readDataSel ::
  (HasCallStack) =>
  -- | Memory
  I.IntMap (BitVector 8) ->
  -- | Address
  BitVector 32 ->
  -- | Byte enables (@SEL@)
  BitVector 4 ->
  -- | Read value, or 'Nothing' if the read is invalid due to an unsupported
  --   value of @SEL@.
  Maybe (BitVector 32)
readDataSel mem addr sel =
  case sel of
    0b0001 -> readByte (addr + 0)
    0b0010 -> readByte (addr + 1)
    0b0100 -> readByte (addr + 2)
    0b1000 -> readByte (addr + 3)
    0b0011 -> readWord (addr + 0)
    0b1100 -> readWord (addr + 2)
    0b1111 -> readDWord addr
    _ -> Nothing
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

writeDataSel ::
  (HasCallStack) =>
  -- | Memory
  I.IntMap (BitVector 8) ->
  -- | Address
  BitVector 32 ->
  -- | Byte enables (SEL)
  BitVector 4 ->
  -- | Value to write
  BitVector 32 ->
  -- | Updated memory
  I.IntMap (BitVector 8)
writeDataSel mem addr sel val =
  case sel of
    0b0001 ->
      I.insert (fromIntegral $ addr + 3) ll mem
    0b0010 ->
      I.insert (fromIntegral $ addr + 2) lh mem
    0b0100 ->
      I.insert (fromIntegral $ addr + 1) hl mem
    0b1000 ->
      I.insert (fromIntegral $ addr + 0) hh mem
    0b0011 ->
      I.insert (fromIntegral $ addr + 3) ll
        $ I.insert (fromIntegral $ addr + 2) lh mem
    0b1100 ->
      I.insert (fromIntegral $ addr + 1) hl
        $ I.insert (fromIntegral $ addr + 0) hh mem
    0b1111 ->
      I.insert (fromIntegral $ addr + 3) ll
        $ I.insert (fromIntegral $ addr + 2) lh
        $ I.insert (fromIntegral $ addr + 1) hl
        $ I.insert (fromIntegral $ addr + 0) hh mem
    _ -> error $ "Got SEL = " <> show sel <> " which is unsupported"
 where
  (hh :: BitVector 8, hl :: BitVector 8, lh :: BitVector 8, ll :: BitVector 8) = unpack val
