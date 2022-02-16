{-|
Copyright  :  (C) 2020, Christiaan Baaij
              (C) 2021, Google LLC
License    :  Apache-2.0
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}
module Contranomy.Core.ALU where

import Clash.Prelude

import Contranomy.Clash.Extra
import Contranomy.Core.Decode
import Contranomy.Core.SharedTypes
import Contranomy.Instruction

-- | The ALU implements the following instructions, or at least parts of them
--
-- * LUI
-- * AUIPC
--
-- * ADD/SUB/ADDI
-- * SLL/SLLI
-- * SLT/SLTI
-- * SLTU/SLTUI
-- * XOR/XORI
-- * SRL/SRLI
-- * SRA/SRAI
-- * OR/ORI
-- * AND/ANDI
--
-- It is additionally used to calculate the PC after the PC of the current instruction for:
--
-- * JAL
-- * JALR
--
-- And it performs the address calculation for the Load and Store instructions
--
-- * LOAD
-- * STORE
alu ::
  -- | Instruction
  MachineWord ->
  -- | Program counter
  PC ->
  -- | Value of RS1
  MachineWord ->
  -- | Value of RS2
  MachineWord ->
  -- | Result
  MachineWord

alu instruction pc rs1Value rs2Value = if isM then multdivResult else aluResult
 where
  DecodedInstruction
    { opcode, iop, srla, isSub, imm12I, imm20U, imm12S, mop, isM, compressed}
    = decodeInstruction instruction

  aluArg1 = case opcode of
              LUI   -> 0
              AUIPC -> pc
              JAL   -> pc
              JALR  -> pc
              _     -> rs1Value
  aluArg2 = case opcode of
              LUI    -> imm20U ++# 0
              AUIPC  -> imm20U ++# 0
              JAL    -> iLEN
              JALR   -> iLEN
              OP     -> case aluOp of
                          ADD | isSub -> negate rs2Value
                          _ -> rs2Value
              STORE  -> signExtend imm12S
              _      -> signExtend imm12I
  iLEN = if compressed then 2 else 4

  aluArg2Shamt = unpack (zeroExtend (slice d4 d0 aluArg2))

  aluOp = case opcode of
            OP     -> iop
            OP_IMM -> iop
            _      -> ADD

  aluResult = case aluOp of
    ADD  -> aluArg1 + aluArg2
    SLL  -> aluArg1 `shiftL` aluArg2Shamt
    SLT  -> boolToBitVector ((unpack aluArg1 :: Signed 32) < unpack aluArg2)
    SLTU -> boolToBitVector (aluArg1 < aluArg2)
    XOR  -> aluArg1 `xor` aluArg2
    SR   -> case srla of
              Logical    -> aluArg1 `shiftR` aluArg2Shamt
              Arithmetic -> pack ((unpack aluArg1 :: Signed 32) `shiftR` aluArg2Shamt)
    OR   -> aluArg1 .|. aluArg2
    AND  -> aluArg1 .&. aluArg2

  -- Alternative operations for the M Extension implemented according to
  -- https://github.com/SymbioticEDA/riscv-formal/blob/master/docs/rvfi.md#alternative-arithmetic-operations
#ifdef FORMAL_ALTOPS
  multdivResult = case mop of
    MUL     -> xor 0x5876063e $ lower $ pack @(Signed 64) $ (getSigned rs1Value + getSigned rs2Value)
    MULH    -> xor 0xf6583fb7 $ lower $ pack @(Signed 64) $ (getSigned rs1Value + getSigned rs2Value)
    MULHSU  -> xor 0xecfbe137 $ lower $ pack @(Signed 64) $ (getSigned rs1Value - (unpack @(Signed 64) $ zeroExtend rs2Value))
    MULHU   -> xor 0x949ce5e8 $ lower $ pack @(Unsigned 64) $ (getUnsigned rs1Value + getUnsigned rs2Value)
    DIV     -> xor 0x7f8529ec $ lower $ pack @(Signed 64) $ (getSigned rs1Value - getSigned rs2Value)
    DIVU    -> xor 0x10e8fd70 $ lower $ pack @(Unsigned 64)$ (getUnsigned rs1Value - getUnsigned rs2Value)
    REM     -> xor 0x8da68fa5 $ lower $ pack @(Signed 64) $ (getSigned rs1Value - getSigned rs2Value)
    REMU    -> xor 0x3138d0e1 $ lower $ pack @(Unsigned 64) $ (getUnsigned rs1Value - getUnsigned rs2Value)
    where
      getUnsigned = zeroExtend . unpack @(Unsigned 32)
      getSigned = signExtend . unpack @(Signed 32)
      lower = slice d31 d0
#else
  multdivResult = case mop of
    MUL     -> lower $ pack @(Signed 64)    $ (getSigned rs1Value * getSigned rs2Value)
    MULH    -> upper $ pack @(Signed 64)    $ (getSigned rs1Value * getSigned rs2Value)
    MULHSU  -> upper $ pack @(Signed 64)    $ (getSigned rs1Value * (unpack @(Signed 64) $ zeroExtend rs2Value))
    MULHU   -> upper $ pack @(Unsigned 64)  $ (getUnsigned rs1Value * getUnsigned rs2Value)
    DIV     -> lower $ pack @(Signed 64)    $ (getSigned rs1Value `quotRisc` getSigned rs2Value)
    DIVU    -> lower $ pack @(Unsigned 64)  $ (getUnsigned rs1Value `quot` getUnsigned rs2Value)
    REM     -> lower $ pack @(Signed 64)    $ (getSigned rs1Value `remRisc` getSigned rs2Value)
    REMU    -> lower $ pack @(Unsigned 64)  $ (getUnsigned rs1Value `rem` getUnsigned rs2Value)
    where
      getUnsigned = zeroExtend . unpack @(Unsigned 32)
      getSigned = signExtend . unpack @(Signed 32)
      lower = slice d31 d0
      upper = slice d63 d32
      quotRisc x y  | y == 0                                = -1
                    | x == minBound @(Signed 64) && y == -1 = y
                    | otherwise                             = x `quot` y
      remRisc x y   | y == 0                                = x
                    | x == minBound @(Signed 64) && y == -1 = -1
                    | otherwise                             = x `rem` y
#endif
