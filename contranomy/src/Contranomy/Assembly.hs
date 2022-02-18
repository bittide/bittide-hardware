{-|
Copyright  :  (C) 2021, Google LLC
License    :  Apache-2.0
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

module Contranomy.Assembly where
import Clash.Prelude
import Contranomy.Core.SharedTypes

-- See Volume I: RISC-V User-Level ISA V2.2:
--   2.2 Base Instruction Formats
--   2.3 Immediate Encoding Variants
--
type Rd = BitVector 5
type Rs1 = BitVector 5
type Rs2 = BitVector 5
type Imm21U = BitVector 32
type Imm21L = BitVector 21
type Imm13 = BitVector 13
type Imm12 = BitVector 12
type Funct3 = BitVector 3
type Funct7 = BitVector 7
type Opcode = BitVector 7
type Pred = BitVector 3
type Succ = BitVector 4
type Shamt = BitVector 5
type Fm = BitVector 5

data Instruction =
    LUI     Rd Imm21U
  | AUIPC   Rd Imm21U
  | JAL     Rd Imm21L
  | JALR    Rd Rs1 Imm12
  | BEQ     Rs1 Rs2 Imm13
  | BNE     Rs1 Rs2 Imm13
  | BLT     Rs1 Rs2 Imm13
  | BGE     Rs1 Rs2 Imm13
  | BLTU    Rs1 Rs2 Imm13
  | BGEU    Rs1 Rs2 Imm13
  | LB      Rd Rs1 Imm12
  | LH      Rd Rs1 Imm12
  | LW      Rd Rs1 Imm12
  | LBU     Rd Rs1 Imm12
  | LHU     Rd Rs1 Imm12
  | SB      Rs1 Rs2 Imm12
  | SH      Rs1 Rs2 Imm12
  | SW      Rs1 Rs2 Imm12
  | ADDI    Rd Rs1 Imm12
  | SLTI    Rd Rs1 Imm12
  | SLTIU   Rd Rs1 Imm12
  | XORI    Rd Rs1 Imm12
  | ORI     Rd Rs1 Imm12
  | ANDI    Rd Rs1 Imm12
  | SLLI    Rd Rs1 Shamt
  | SRLI    Rd Rs1 Shamt
  | SRAI    Rd Rs1 Shamt
  | ADD     Rd Rs1 Rs2
  | SUB     Rd Rs1 Rs2
  | SLL     Rd Rs1 Rs2
  | SLT     Rd Rs1 Rs2
  | SLTU    Rd Rs1 Rs2
  | XOR     Rd Rs1 Rs2
  | SRL     Rd Rs1 Rs2
  | SRA     Rd Rs1 Rs2
  | OR      Rd Rs1 Rs2
  | AND     Rd Rs1 Rs2
  | FENCE   Rd Rs1 Pred Succ Fm
  | ECALL
  | EBREAK
  | MUL     Rd Rs1 Rs2  -- M Extension
  | MULH    Rd Rs1 Rs2  -- M Extension
  | MULHSU  Rd Rs1 Rs2  -- M Extension
  | MULHU   Rd Rs1 Rs2  -- M Extension
  | DIV     Rd Rs1 Rs2  -- M Extension
  | DIVU    Rd Rs1 Rs2  -- M Extension
  | REM     Rd Rs1 Rs2  -- M Extension
  | REMU    Rd Rs1 Rs2  -- M Extension
  deriving Show

data Encoding =
   RType Opcode Rd Rs1 Rs2 Funct3 Funct7
 | IType Opcode Rd Rs1 Funct3 Imm12
 | SType Opcode Rs1 Rs2 Funct3 Imm12
 | BType Opcode Rs1 Rs2 Funct3 Imm13
 | UType Opcode Rd Imm21U
 | JType Opcode Rd Imm21L


instructionToMachineword :: Instruction -> MachineWord
instructionToMachineword = formatToMachineWord . instructionToFormat

formatToMachineWord :: Encoding -> MachineWord
formatToMachineWord = \case
  RType opcode rd rs1 rs2 funct3 funct7 ->
    funct7 ++# rs2 ++# rs1 ++# funct3 ++# rd ++# opcode

  IType opcode rd rs1 funct3 imm ->
    imm ++# rs1 ++# funct3 ++# rd ++# opcode

  SType opcode rs1 rs2 funct3 imm ->
    slice d11 d5 imm ++# rs2 ++# rs1 ++# funct3 ++# slice d4 d0 imm ++# opcode

  BType opcode rs1 rs2 funct3 imm ->
    slice d12 d12 imm ++# slice d10 d5 imm ++# rs2 ++# rs1 ++# funct3 ++#
    slice d4 d1 imm ++# slice d11 d11 imm ++# opcode

  UType opcode rd imm ->
    slice d31 d12 imm ++# rd ++# opcode

  JType opcode rd imm ->
    slice d20 d20 imm ++# slice d10 d1 imm ++# slice d11 d11 imm ++#
    slice d19 d12 imm ++# rd ++# opcode

instructionToFormat :: Instruction -> Encoding
instructionToFormat = \case
  -- Constructors are in order of specification:
  --   Volume I: RISC-V User-Level ISA V2.2
  --   Chapter 19: RV32/64G Instruction Set Listings
  --   Tables: RV32I Base Instruction Set
  --           RV32M Standard Extension
  LUI     rd imm       -> UType 0b0110111 rd imm
  AUIPC   rd imm       -> UType 0b0010111 rd imm
  JAL     rd imm       -> JType 0b1100111 rd imm
  JALR    rd rs1 imm   -> IType 0b1101111 rd rs1 0b000 imm
  BEQ     rs1 rs2 imm  -> BType 0b1100011 rs1 rs2 0b000 imm
  BNE     rs1 rs2 imm  -> BType 0b1100011 rs1 rs2 0b001 imm
  BLT     rs1 rs2 imm  -> BType 0b1100011 rs1 rs2 0b100 imm
  BGE     rs1 rs2 imm  -> BType 0b1100011 rs1 rs2 0b101 imm
  BLTU    rs1 rs2 imm  -> BType 0b1100011 rs1 rs2 0b110 imm
  BGEU    rs1 rs2 imm  -> BType 0b1100011 rs1 rs2 0b111 imm
  LB      rd rs1 imm   -> IType 0b0000011 rd rs1 0b000 imm
  LH      rd rs1 imm   -> IType 0b0000011 rd rs1 0b001 imm
  LW      rd rs1 imm   -> IType 0b0000011 rd rs1 0b010 imm
  LBU     rd rs1 imm   -> IType 0b0000011 rd rs1 0b100 imm
  LHU     rd rs1 imm   -> IType 0b0000011 rd rs1 0b101 imm
  SB      rs1 rs2 imm  -> SType 0b0100011 rs1 rs2 0b000 imm
  SH      rs1 rs2 imm  -> SType 0b0100011 rs1 rs2 0b001 imm
  SW      rs1 rs2 imm  -> SType 0b0100011 rs1 rs2 0b010 imm
  ADDI    rd rs1 imm   -> IType 0b0010011 rd rs1 0b000 imm
  SLTI    rd rs1 imm   -> IType 0b0010011 rd rs1 0b010 imm
  SLTIU   rd rs1 imm   -> IType 0b0010011 rd rs1 0b011 imm
  XORI    rd rs1 imm   -> IType 0b0010011 rd rs1 0b100 imm
  ORI     rd rs1 imm   -> IType 0b0010011 rd rs1 0b110 imm
  ANDI    rd rs1 imm   -> IType 0b0010011 rd rs1 0b111 imm
  SLLI    rd rs1 shamt -> RType 0b0010011 rd rs1 shamt 0b001 0b0000000
  SRLI    rd rs1 shamt -> RType 0b0010011 rd rs1 shamt 0b101 0b0000000
  SRAI    rd rs1 shamt -> RType 0b0010011 rd rs1 shamt 0b101 0b0100000
  ADD     rd rs1 rs2   -> RType 0b0110011 rd rs1 rs2 0b000 0b0000000
  SUB     rd rs1 rs2   -> RType 0b0110011 rd rs1 rs2 0b000 0b0100000
  SLL     rd rs1 rs2   -> RType 0b0110011 rd rs1 rs2 0b001 0b0000000
  SLT     rd rs1 rs2   -> RType 0b0110011 rd rs1 rs2 0b010 0b0000000
  SLTU    rd rs1 rs2   -> RType 0b0110011 rd rs1 rs2 0b011 0b0000000
  XOR     rd rs1 rs2   -> RType 0b0110011 rd rs1 rs2 0b100 0b0000000
  SRL     rd rs1 rs2   -> RType 0b0110011 rd rs1 rs2 0b101 0b0000000
  SRA     rd rs1 rs2   -> RType 0b0110011 rd rs1 rs2 0b101 0b0100000
  OR      rd rs1 rs2   -> RType 0b0110011 rd rs1 rs2 0b110 0b0000000
  AND     rd rs1 rs2   -> RType 0b0110011 rd rs1 rs2 0b111 0b0000000
  FENCE   rd rs1 pred' succ' fm ->
    IType 0b0001111 rd rs1 0 (fm ++# pred' ++# succ')
  ECALL                -> IType 0b1110011 0 0 0 0
  EBREAK               -> IType 0b1110011 0 0 0 1
  MUL     rd rs1 rs2   -> RType 0b0110011 rd rs1 rs2 0b000 0b0000001
  MULH    rd rs1 rs2   -> RType 0b0110011 rd rs1 rs2 0b001 0b0000001
  MULHSU  rd rs1 rs2   -> RType 0b0110011 rd rs1 rs2 0b010 0b0000001
  MULHU   rd rs1 rs2   -> RType 0b0110011 rd rs1 rs2 0b011 0b0000001
  DIV     rd rs1 rs2   -> RType 0b0110011 rd rs1 rs2 0b100 0b0000001
  DIVU    rd rs1 rs2   -> RType 0b0110011 rd rs1 rs2 0b101 0b0000001
  REM     rd rs1 rs2   -> RType 0b0110011 rd rs1 rs2 0b110 0b0000001
  REMU    rd rs1 rs2   -> RType 0b0110011 rd rs1 rs2 0b111 0b0000001
