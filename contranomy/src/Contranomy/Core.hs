-- SPDX-FileCopyrightText: 2022 Google LLC
-- SPDX-FileCopyrightText: 2020 Christiaan Baaij
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module Contranomy.Core where

import Control.Lens
import Data.Generics.Labels ()
import Data.Maybe
import Control.Monad
import Clash.Prelude

import Contranomy.Clash.Extra

import Bittide.Extra.Wishbone
import Contranomy.Core.ALU
import Contranomy.Core.CoreState
import Contranomy.Core.CSR
import Contranomy.Core.Decode
import Contranomy.Core.Exception
import Contranomy.Core.Branch
import Contranomy.Core.LoadStore
import Contranomy.Core.MachineState
import Contranomy.Core.RVFI
import Contranomy.Core.SharedTypes

import Contranomy.Instruction
import Contranomy.RVFI

type TimerInterrupt = Bool
type SoftwareInterrupt = Bool
type ExternalInterrupt = MachineWord

data CoreIn
  = CoreIn
  { iBusS2M :: "iBusWishbone" ::: WishboneS2M Bytes
  , dBusS2M :: "dBusWishbone" :::  WishboneS2M Bytes
  , timerInterrupt :: "timerInterrupt" ::: TimerInterrupt
  , softwareInterrupt :: "softwareInterrupt" ::: SoftwareInterrupt
  , externalInterrupt :: "externalInterrupt" ::: ExternalInterrupt
  }

data CoreOut
  = CoreOut
  { iBusM2S :: "iBusWishbone" ::: WishboneM2S Bytes AddressWidth
  , dBusM2S :: "dBusWishbone" ::: WishboneM2S Bytes AddressWidth
  }

coreOut :: CoreOut
coreOut = CoreOut
  { iBusM2S = emptyWishboneM2S @Bytes @AddressWidth
  , dBusM2S = emptyWishboneM2S @Bytes @AddressWidth
  }

{-# NOINLINE core #-}
core ::
  HiddenClockResetEnable dom =>
  BitVector 32 ->
  (Signal dom CoreIn, Signal dom (MachineWord, MachineWord)) ->
  ( Signal dom CoreOut
  , Signal dom (Maybe Register, Maybe Register, Maybe (Register, MachineWord))
  , Signal dom RVFI )
core entry = mealyAutoB transition cpuStart
 where
  cpuStart
    = CoreState
    { stage = InstructionFetch
    , pc = entry
    , instruction = 0
    , machineState = machineStart
    , rvfiOrder = 0
    }

  machineStart
    = MachineState
    { mstatus = MStatus { mie = False, mpie = False }
    , mcause = MCause { interrupt = False, code = 0 }
    , mtvec = Direct 0
    , mie = Mie { meie = False, mtie = False, msie = False }
    , mscratch = 0
    , mepc = 0
    , mtval = 0
    , irqmask = 0
    }

transition ::
  CoreState ->
  (CoreIn, (MachineWord, MachineWord)) ->
  ( (CoreOut, (Maybe Register,Maybe Register, Maybe (Register, MachineWord)), RVFI)
  , CoreState )
-- Fetch + Decode
transition s@CoreState{stage=InstructionFetch, pc} (CoreIn{iBusS2M},_) = runState' s do

  #instruction .= readData iBusS2M
  let DecodedInstruction {rs1,rs2} = decodeInstruction (readData iBusS2M)

  #stage .= if err iBusS2M then
              Execute {accessFault = True}
            else if acknowledge iBusS2M then
              Execute {accessFault = False}
            else
              InstructionFetch

  return ( coreOut { iBusM2S = (emptyWishboneM2S @Bytes @AddressWidth)
                             { addr = pc
                             , busSelect = 0b1111
                             , busCycle = True
                             , strobe = True } }

         , (Just rs1 , Just rs2 , Nothing)

         , rvfi
         )

-- Execute + Writeback
transition
    s@CoreState{stage=Execute accessFault,instruction,pc,rvfiOrder}
    ( CoreIn{dBusS2M,softwareInterrupt,timerInterrupt,externalInterrupt}
    , (rs1Val,rs2Val) )
  = runState' s do

  let DecodedInstruction { opcode, rd, legal }
        = decodeInstruction instruction

  let aluIResult = alu instruction pc rs1Val rs2Val

      instructionFault = accessFault || not legal

      (dBusM2S,ldVal,dataAccessFault,dataAddrMisaligned,loadStoreFinished) =
        loadStoreUnit instruction instructionFault aluIResult rs2Val dBusS2M

  let pcN = branchUnit instruction rs1Val rs2Val pc
  let (_,alignment :: Alignment) = split pcN
  let exceptionIn
        = ExceptionIn
        { instrAccessFault = accessFault
        , instrAddrMisaligned = alignment /= 0
        , instrIllegal = not legal
        , dataAccessFault = dataAccessFault
        , dataAddrMisaligned = dataAddrMisaligned
        , timerInterrupt = timerInterrupt
        , softwareInterrupt = softwareInterrupt
        , externalInterrupt = externalInterrupt
        }
  (trap,pcN1) <- handleExceptions s exceptionIn loadStoreFinished pcN

  csrVal@(csrOld,_) <- zoom #machineState $
    csrUnit trap instruction rs1Val softwareInterrupt timerInterrupt externalInterrupt

  let rdVal = case opcode of
        BRANCH   -> Nothing
        MISC_MEM -> Nothing
        STORE    -> Nothing
        SYSTEM   -> csrOld
        LOAD     -> ldVal
        _        -> Just aluIResult

  let registerWrite =
        if trap || rd == X0 then
          Nothing
        else (rd,) <$> rdVal

  when loadStoreFinished do
    #pc .= pcN1
    #rvfiOrder += 1
    #stage .= InstructionFetch

  let rvfiOut = toRVFI loadStoreFinished rvfiOrder instruction trap rs1Val rs2Val
                    registerWrite pc pcN1 dBusM2S dBusS2M csrVal

  return ( coreOut { dBusM2S = dBusM2S }

         , (Nothing,Nothing,registerWrite)

         , rvfiOut
         )
