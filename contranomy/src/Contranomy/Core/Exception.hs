-- SPDX-FileCopyrightText: 2022 Google LLC
-- SPDX-FileCopyrightText: 2020 Christiaan Baaij
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Contranomy.Core.Exception where

import Control.Lens
import Control.Monad.Trans.State
import Data.Maybe
import Data.Generics.Labels ()

import Clash.Prelude

import Contranomy.Core.CoreState
import Contranomy.Core.Decode
import Contranomy.Core.MachineState
import Contranomy.Core.SharedTypes
import Contranomy.Instruction

data ExceptionIn
  = ExceptionIn
  { instrAccessFault    :: Bool
  , instrAddrMisaligned :: Bool
  , instrIllegal        :: Bool
  , dataAccessFault     :: Maybe MachineWord
  , dataAddrMisaligned  :: Maybe MachineWord
  , timerInterrupt      :: Bool
  , softwareInterrupt   :: Bool
  , externalInterrupt   :: MachineWord
  }

{-# NOINLINE handleExceptions #-}
-- | This function takes care of exception handling, both synchronous exceptions
-- (traps) and asynchronous exceptions (interrupts). It takes in information
-- from other functional units whether to raise a trap.
--
-- It updates all the relevant Machine CSRs when a trap or interrupt is enterred
-- and then returns the PC of the trap handler (stored in mtvec).
--
-- This function also implements the MRET, machine trap return, instruction
-- since we need to update the mstatus CSR, and return the PC from before the
-- trap was entered (stored in mepc)
handleExceptions ::
  CoreState ->
  ExceptionIn ->
  -- | Load/Store unit finished
  Bool ->
  -- | Next PC
  PC ->
  -- |
  -- 1. Indication whether a trap or interrupt was raised
  -- 2. The PC for the next instruction cycle
  State CoreState (Bool,PC)
handleExceptions CoreState{pc,instruction,machineState} exceptionIn lsFinished pcN = do
  let ExceptionIn
        { instrAccessFault
        , instrAddrMisaligned
        , instrIllegal
        , dataAccessFault
        , dataAddrMisaligned
        , timerInterrupt
        , softwareInterrupt
        , externalInterrupt
        } = exceptionIn

  let DecodedInstruction{opcode,func3,imm12I} = decodeInstruction instruction
  let breakpoint = case opcode of
        SYSTEM
          | func3 == 0
          -> System12 imm12I == EBREAK
        _ -> False

  let eCall = case opcode of
        SYSTEM
          | func3 == 0
          -> System12 imm12I == ECALL
        _ -> False

  let mret = case opcode of
        SYSTEM
          | func3 == 0
          -> System12 imm12I == MRET
        _ -> False

  let trap = instrAccessFault || instrAddrMisaligned || instrIllegal ||
        isJust dataAccessFault || isJust dataAddrMisaligned || breakpoint || eCall

  let MachineState{mstatus,mie=Mie{mtie,msie,meie},mepc,mtvec,irqmask} = machineState
      MStatus{mie,mpie} = mstatus

      timerInterrupt1    = timerInterrupt && mtie
      softwareInterrupt1 = softwareInterrupt && msie
      externalInterrupt1 = ((externalInterrupt .&. irqmask) /= 0) && meie
  let interrupt =
        lsFinished && mie && (timerInterrupt1 || softwareInterrupt1 || externalInterrupt1)


  if trap || interrupt then do
    #machineState .= machineState
                    { mstatus = MStatus { mpie = mie, mie = False }
                    , mcause =
                      if interrupt then
                        if softwareInterrupt1 then
                          MACHINE_SOFTWARE_INTERRUPT
                        else if timerInterrupt1 then
                          MACHINE_TIMER_INTERRUPT
                        else -- externalInterrupt1
                          MACHINE_EXTERNAL_INTERRUPT
                      else
                        if instrAccessFault then
                          INSTRUCTION_ACCESS_FAULT
                        else if instrIllegal then
                          ILLEGAL_INSTRUCTION
                        else if instrAddrMisaligned then
                          INSTRUCTION_ADDRESS_MISALIGNED
                        else if eCall then
                          ENVIRONMENT_CALL
                        else if breakpoint then
                          BREAKPOINT
                        else case dataAddrMisaligned of
                          Just _ -> case opcode of
                            LOAD -> LOAD_ADDRESS_MISALIGNED
                            _ -> STORE_ADDRESS_MISALIGNED
                          _ -> case opcode of -- dataAccessFault
                            LOAD -> LOAD_ADDRESS_MISALIGNED
                            _ -> STORE_ADDRESS_MISALIGNED
                    , mepc = slice d31 d2 pc ++# ( 0 :: BitVector 2)
                    , mtval =
                      if instrAddrMisaligned then
                        pcN
                      else if instrIllegal then
                        instruction
                      else if instrAccessFault then
                        slice d31 d2 pc ++# 0
                      else if breakpoint then
                        slice d31 d2 pc ++# 0
                      else case dataAddrMisaligned of
                        Just addr -> addr
                        _ -> case dataAccessFault of
                          Just addr -> addr
                          _ -> 0
                    }
    let pcN1 = trapBase mtvec
        pcN2 = pcN1 ++# (0:: BitVector 2)
        pcN3 = slice d31 d2 pcN2 ++# (0 ::BitVector 2)
    return (True,pcN3)
  else if mret then do
    #machineState .= machineState { mstatus = mstatus {mie = mpie} }
    return (False,alignPC mepc)
  else do
    return (False, pcN)
