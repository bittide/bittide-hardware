-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

{- |
See: http://cdn.opencores.org/downloads/wbspec_b4.pdf
-}
module Bittide.Extra.Wishbone where

import Clash.Prelude
import Clash.Signal.Internal

import Protocols
import Protocols.Wishbone

import qualified Data.IntMap as I

type DWord = BitVector (4 * 8)

{- | The wishbone storage is a simulation only memory element that communicates via the
Wishbone protocol : http://cdn.opencores.org/downloads/wbspec_b4.pdf .
It receives a name for error identification, an Intmap of BitVector 8 as initial content.
The storage is byte addressable.
-}
wishboneStorage ::
  String ->
  I.IntMap (BitVector 8) ->
  Circuit (Wishbone dom 'Standard 32 DWord) ()
wishboneStorage name initial =
  Circuit $ \(input, ()) -> (wishboneStorage' name state input, ())
 where
  state = (initial, False)

wishboneStorage' ::
  String ->
  (I.IntMap (BitVector 8), Bool) ->
  Signal dom (WishboneM2S 32 (BitSize DWord `DivRU` 8) DWord) ->
  Signal dom (WishboneS2M DWord)
wishboneStorage' name state inputs = dataOut :- (wishboneStorage' name state' inputs')
 where
  input :- inputs' = inputs
  state' = (file', ack')
  (file, ack) = state
  WishboneM2S
    { addr
    , writeData
    , busSelect
    , busCycle
    , strobe
    , writeEnable
    } = input
  file'
    | writeEnable = I.fromList assocList <> file
    | otherwise = file
  ack' = busCycle && strobe
  address = fromIntegral (unpack $ addr :: Unsigned 32)
  readData =
    if not writeEnable
      then
        (file `lookup'` (address + 3))
          ++# (file `lookup'` (address + 2))
          ++# (file `lookup'` (address + 1))
          ++# (file `lookup'` address)
      else 0
  lookup' x addr' =
    I.findWithDefault
      (error $ name <> ": Uninitialized Memory Address = " <> show addr')
      addr'
      x
  assocList = case busSelect of
    $(bitPattern "0001") -> [byte0]
    $(bitPattern "0010") -> [byte1]
    $(bitPattern "0100") -> [byte2]
    $(bitPattern "1000") -> [byte3]
    $(bitPattern "0011") -> half0
    $(bitPattern "1100") -> half1
    _ -> word0
  byte0 = (address, slice d7 d0 writeData)
  byte1 = (address + 1, slice d15 d8 writeData)
  byte2 = (address + 2, slice d23 d16 writeData)
  byte3 = (address + 3, slice d31 d24 writeData)
  half0 = [byte0, byte1]
  half1 = [byte2, byte3]
  word0 = [byte0, byte1, byte2, byte3]
  dataOut =
    (emptyWishboneS2M @DWord)
      { readData = readData
      , acknowledge = ack
      , err = False
      }

{- | Wrapper for the wishboneStorage that allows two ports to be connected.
Port A can only be used for reading, port B can read and write to the te storage.
Writing from port A is illegal and write attempts will set the err signal.
-}
instructionStorage ::
  String ->
  I.IntMap (BitVector 8) ->
  Circuit (Wishbone dom 'Standard 32 DWord, Wishbone dom 'Standard 32 DWord) ()
instructionStorage name initial = Circuit go
 where
  go ((aM2S, bM2S), ()) = ((aS2M, bS2M), ())
   where
    Circuit storageFn = wishboneStorage name initial
    (storageOut, ()) = storageFn (storageIn, ())
    aActive = strobe <$> aM2S .&&. busCycle <$> aM2S
    bActive = strobe <$> bM2S .&&. busCycle <$> bM2S
    aWriting = aActive .&&. writeEnable <$> aM2S

    storageIn = mux (not <$> bActive) (noWrite <$> aM2S) bM2S

    aS2M = mux (not <$> bActive) (writeIsErr <$> storageOut <*> aWriting) (noAck <$> storageOut)
    bS2M = storageOut

    noAck wb = wb{acknowledge = False, err = False}
    noWrite wb = wb{writeEnable = False}
    writeIsErr wb write = wb{err = err wb || write}
