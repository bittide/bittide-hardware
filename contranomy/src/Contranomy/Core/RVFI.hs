-- SPDX-FileCopyrightText: 2022 Google LLC
-- SPDX-FileCopyrightText: 2020 Christiaan Baaij
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NamedFieldPuns #-}

module Contranomy.Core.RVFI where

import Clash.Prelude

import Bittide.Extra.Wishbone
import Contranomy.Core.Decode
import Contranomy.Core.SharedTypes
import Contranomy.Instruction
import Contranomy.RVFI

toRVFI ::
  -- | Load/Store unit has finished
  Bool ->
  -- rvfiOrder
  Unsigned 64 ->
  -- | instruction
  MachineWord ->
  -- | trap
  Bool ->
  -- | rs1 value
  MachineWord ->
  -- | rs2 value
  MachineWord ->
  -- | rdVal
  Maybe (Register,MachineWord) ->
  -- | pc
  PC ->
  -- | pcN
  PC ->
  -- | dbusM2S
  WishboneM2S Bytes AddressWidth ->
  -- | dbusS2M
  WishboneS2M Bytes ->
  -- | MISA CRS
  (Maybe MachineWord, MachineWord) ->
  RVFI
toRVFI loadStoreFinished rvfiOrder instruction trap rs1Val rs2Val rdVal pc pcN dBusM2S dBusS2M csrVal
  = rvfi
  { valid    = loadStoreFinished
  , order    = rvfiOrder
  , insn     = instruction
  , trap     = trap
  , rs1Addr  = rs1
  , rs2Addr  = rs2
  , rs1RData = rs1Val
  , rs2RData = rs2Val
  , rdAddr   = maybe X0 fst rdVal
  , rdWData  = maybe 0 snd rdVal
  , pcRData  = pc
  , pcWData  = pcN
  , memAddr  = if trap then 0 else addr dBusM2S
  , memRMask = if strobe dBusM2S && not (writeEnable dBusM2S) then
                  busSelect dBusM2S
                else
                  0
  , memWMask = if strobe dBusM2S && writeEnable dBusM2S then
                  busSelect dBusM2S
                else
                  0
  , memRData = if strobe dBusM2S && not (writeEnable dBusM2S) then
                  readData dBusS2M
                else
                  0
  , memWData = if strobe dBusM2S && writeEnable dBusM2S then
                  writeData dBusM2S
                else
                  0
  , misaCSR  = case csrVal of
      (Just old,newVal)
        | MISA <- CSRRegister srcDest
        -> RVFICSR { rmask = maxBound
                   , wmask = maxBound
                   , rdata = old
                   , wdata = newVal
                   }
      _ -> rvfiCSR {  rmask = maxBound
                    , rdata = bit 30 .|. bit 8 .|. bit 2 .|. bit 12}
  }
 where
  DecodedInstruction {rs1,rs2,imm12I=srcDest} = decodeInstruction instruction
