-- SPDX-FileCopyrightText: 2022 Google LLC
-- SPDX-FileCopyrightText: 2020 Christiaan Baaij
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NamedFieldPuns #-}

module Contranomy.Core.LoadStore where

import Clash.Prelude

import Bittide.Extra.Wishbone
import Contranomy.Core.Decode
import Contranomy.Core.SharedTypes
import Contranomy.Instruction

{-# NOINLINE loadStoreUnit #-}
-- | This function performs data-bus transactions for loads and stores.
--
-- It does not initiate the transaction if the address is misaligned for the
-- given load/store size, nor does it perform bus transactions for faulty
-- instructions (not even loads, they might have side-effects).
loadStoreUnit ::
  -- | Instruction
  MachineWord ->
  -- | Instruction faulty, no data bus transactions should happen for faulty instrucctions
  Bool ->
  -- | Load/Store address (calculated by the ALU)
  MachineWord ->
  -- | The value to store
  MachineWord ->
  -- | Data bus response (slave-to-master)
  WishboneS2M Bytes ->
  -- |
  -- 1. Data bus initiation (master-to-slave)
  -- 2. The address causing a data-access fault on the data bus
  -- 3. The misaligned address
  -- 4. Data bus transaction completed
  (WishboneM2S Bytes AddressWidth, Maybe MachineWord, Maybe MachineWord, Maybe MachineWord, Bool)
loadStoreUnit instruction instructionFault addr toStore dBusS2M = case opcode of
  LOAD | not instructionFault ->
    let loadData = case loadStoreWidth of
          Byte sign ->
            loadExtend
              sign
              (slice d7 d0 (readData dBusS2M `shiftR` shiftAmount))
          Half sign ->
            loadExtend
              sign
              (slice d15 d0 (readData dBusS2M `shiftR` shiftAmount))
          _ -> readData dBusS2M
     in ( (emptyWishboneM2S @Bytes @AddressWidth)
            { addr      = slice d31 d2 addr ++# (0 :: BitVector 2)
            , busSelect = mask
            , busCycle  = aligned
            , strobe    = aligned
            }
        , if not aligned || err dBusS2M || not (acknowledge dBusS2M) then
             Nothing
          else
            Just loadData
        , if err dBusS2M then Just addr else Nothing
        , if aligned then Nothing else Just addr
        , not aligned || busFinished
        )

  STORE | not instructionFault ->
    let storeData = case loadStoreWidth of
          Byte _ -> toStore `shiftL` shiftAmount
          Half _ -> toStore `shiftL` shiftAmount
          _ -> toStore
     in ( (emptyWishboneM2S @Bytes @AddressWidth)
           { addr        = slice d31 d2 addr ++# (0 :: BitVector 2)
           , writeData   = storeData
           , busSelect   = mask
           , busCycle    = aligned
           , strobe      = aligned
           , writeEnable = aligned
           }
        , Nothing
        , if err dBusS2M then Just addr else Nothing
        , if aligned then Nothing else Just addr
        , not aligned || busFinished
        )

  _otherwise ->
    ( emptyWishboneM2S @Bytes @AddressWidth
    , Nothing
    , Nothing
    , Nothing
    , True
    )
 where
  DecodedInstruction {opcode,func3} = decodeInstruction instruction

  busFinished = err dBusS2M || acknowledge dBusS2M

  loadStoreWidth :: LoadStoreWidth
  loadStoreWidth = unpack func3

  alignment = slice d1 d0 addr

  aligned = case loadStoreWidth of
    Word
      -> alignment == 0
    Half _
      -> not (testBit alignment 0)
    _word
      -> True

  mask = case loadStoreWidth of
    Byte _ -> case alignment of
      3 -> 0b1000
      2 -> 0b0100
      1 -> 0b0010
      _ -> 0b0001
    Half _ -> case alignment of
      2 -> 0b1100
      _ -> 0b0011
    _word
        -> 0b1111

  shiftAmount = case loadStoreWidth of
    Byte _ -> case alignment of
      3 -> 24
      2 -> 16
      1 -> 8
      _ -> 0
    Half _ -> case alignment of
      2 -> 16
      _ -> 0
    _word
        -> 0

loadExtend ::
  (KnownNat n, n <= 32) =>
  Sign ->
  BitVector (32 - n) ->
  MachineWord
loadExtend Unsigned = zeroExtend
loadExtend Signed   = signExtend
