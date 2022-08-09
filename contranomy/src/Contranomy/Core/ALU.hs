-- SPDX-FileCopyrightText: 2022 Google LLC
-- SPDX-FileCopyrightText: 2020 Christiaan Baaij
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}
module Contranomy.Core.ALU
  ( alu

  -- * Internal
  , multdiv
  , multdivSim
  , multdivFormal
  ) where

import Clash.Prelude

import Contranomy.Clash.Extra
import Contranomy.Core.Decode
import Contranomy.Core.SharedTypes
import Contranomy.Instruction

{-# NOINLINE alu #-}
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

alu instruction pc rs1Value rs2Value
  | isM       = multdiv rs1Value rs2Value mop
  | otherwise = aluResult
 where
  DecodedInstruction
    {opcode, iop, srla, isSub, imm12I, imm20U, imm12S, mop, isM, compressed}
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

getUnsigned :: BitVector 32 -> Unsigned 64
getUnsigned = zeroExtend . unpack @(Unsigned 32)

getSigned :: BitVector 32 -> Signed 64
getSigned = signExtend . unpack @(Signed 32)

lower :: BitVector 64 -> BitVector 32
lower = slice d31 d0

upper :: BitVector 64 -> BitVector 32
upper = slice d63 d32

multdiv :: BitVector 32 -> BitVector 32 -> MOp -> BitVector 32
#ifdef FORMAL_ALTOPS
multdiv = multdivFormal
#else
multdiv = multdivSim
#endif

-- | Alternative operations for the M Extension implemented according to
-- https://github.com/SymbioticEDA/riscv-formal/blob/master/docs/rvfi.md#alternative-arithmetic-operations
multdivFormal :: BitVector 32 -> BitVector 32 -> MOp -> BitVector 32
multdivFormal rs1 rs2 = \case
  MUL    -> xor 0x5876063e $ lower $ pack $ getSigned rs1 + getSigned rs2
  MULH   -> xor 0xf6583fb7 $ lower $ pack $ getSigned rs1 + getSigned rs2
  MULHSU -> xor 0xecfbe137 $ lower $ pack $ getSigned rs1 - unpack (zeroExtend rs2)
  MULHU  -> xor 0x949ce5e8 $ lower $ pack $ getUnsigned rs1 + getUnsigned rs2
  DIV    -> xor 0x7f8529ec $ lower $ pack $ getSigned rs1 - getSigned rs2
  DIVU   -> xor 0x10e8fd70 $ lower $ pack $ getUnsigned rs1 - getUnsigned rs2
  REM    -> xor 0x8da68fa5 $ lower $ pack $ getSigned rs1 - getSigned rs2
  REMU   -> xor 0x3138d0e1 $ lower $ pack $ getUnsigned rs1 - getUnsigned rs2

multdivSim :: BitVector 32 -> BitVector 32 -> MOp -> BitVector 32
multdivSim rs1 rs2 = \case
  MUL    -> lower $ pack $ getSigned rs1 * getSigned rs2
  MULH   -> upper $ pack $ getSigned rs1 * getSigned rs2
  MULHSU -> upper $ pack $ getSigned rs1 * unpack (zeroExtend rs2)
  MULHU  -> upper $ pack $ getUnsigned rs1 * getUnsigned rs2

  DIV    -> lower $ pack $ getSigned rs1   `signedQuot`   getSigned rs2
  DIVU   -> lower $ pack $ getUnsigned rs1 `unsignedQuot` getUnsigned rs2
  REM    -> lower $ pack $ getSigned rs1   `signedRem`    getSigned rs2
  REMU   -> lower $ pack $ getUnsigned rs1 `unsignedRem`  getUnsigned rs2

unsignedQuot :: KnownNat n => Unsigned n -> Unsigned n -> Unsigned n
unsignedQuot _ 0 = maxBound
unsignedQuot x y = x `quot` y

unsignedRem :: KnownNat n => Unsigned n -> Unsigned n -> Unsigned n
unsignedRem x 0 = x
unsignedRem x y = x `rem` y

signedQuot :: KnownNat n => Signed n -> Signed n -> Signed n
signedQuot x y
  | y == 0                   = -1
  | x == minBound && y == -1 = x
  | otherwise                = x `quot` y

signedRem :: KnownNat n => Signed n -> Signed n -> Signed n
signedRem x y
  | y == 0                   = x
  | x == minBound && y == -1 = -1
  | otherwise                = x `rem` y
