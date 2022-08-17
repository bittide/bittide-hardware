-- SPDX-FileCopyrightText: 2022 Google LLC
-- SPDX-FileCopyrightText: 2020 Christiaan Baaij
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Contranomy.Core.Decode where

import Clash.Prelude

import Contranomy.Instruction
import Contranomy.Core.SharedTypes

data DecodedInstruction
  = DecodedInstruction
  { opcode      :: Opcode
  , rd          :: Register
  , rs1         :: Register
  , rs2         :: Register
  , iop         :: IOp
  , srla        :: ShiftRight
  , shamt       :: BitVector 5
  , isSub       :: Bool
  , isM         :: Bool
  , mop         :: MOp
  , imm20U      :: BitVector 20
  , imm20J      :: BitVector 20
  , imm12I      :: BitVector 12
  , imm12S      :: BitVector 12
  , imm12B      :: BitVector 12
  , func3       :: BitVector 3
  , compressed  :: Bool
  , legal       :: Bool
  } deriving (Show, Generic, NFDataX)

{-# NOINLINE decodeInstruction #-}
decodeInstruction :: MachineWord -> DecodedInstruction
decodeInstruction w = if slice d1 d0 w == 3 then decode32 w else compressedToFull $ decode16 w

decode32 :: MachineWord -> DecodedInstruction
decode32 w = DecodedInstruction
  { opcode = opcode
  , rd     = unpack (slice d11 d7 w)
  , rs1    = unpack (slice d19 d15 w)
  , rs2    = unpack (slice d24 d20 w)
  , iop    = unpack (slice d14 d12 w)
  , srla   = unpack (slice d30 d30 w)
  , shamt  = unpack (slice d24 d20 w)
  , isSub  = unpack (slice d30 d30 w)
  , isM    = (pack opcode, func7) == (0b0110011, 0b0000001)
  , mop    = unpack (slice d14 d12 w)
  , imm20U = slice d31 d12 w
  , imm20J = slice d31 d31 w ++#
             slice d19 d12 w ++#
             slice d20 d20 w ++#
             slice d30 d21 w

  , imm12I = slice d31 d20 w
  , imm12S = slice d31 d25 w ++# slice d11 d7 w
  , imm12B = slice d31 d31 w ++#
             slice  d7  d7 w ++#
             slice d30 d25 w ++#
             slice d11  d8 w
  , func3  = func3
  , compressed = False
  , legal  = case opcode of
      LUI -> True
      AUIPC -> True
      JAL -> True
      JALR -> func3 == 0
      BRANCH -> case unpack func3 of
        BEQ -> True
        BNE -> True
        BLT -> True
        BGE -> True
        BLTU -> True
        BGEU -> True
        _ -> False
      LOAD -> case unpack func3 of
        Byte _ -> True
        Half _ -> True
        Word -> True
        _ -> False
      STORE -> case unpack func3 of
        Byte Signed -> True
        Half Signed -> True
        Word -> True
        _ -> False
      OP_IMM -> case unpack func3 of
        SR -> func7 == 0 ||        -- SRL
              func7 == 0b010_0000  -- SRA
        _ -> True
      OP -> (func7 == 1) || case unpack func3 of
        ADD -> func7 == 0 ||        -- ADD
               func7 == 0b010_0000  -- SUB
        SLL -> func7 == 0
        SLT -> func7 == 0
        SLTU -> func7 == 0
        XOR -> func7 == 0
        SR -> func7 == 0 ||        -- SRL
              func7 == 0b010_0000  -- SRA
        OR -> func7 == 0
        AND -> func7 == 0
      MISC_MEM -> func3 < 2 -- FENCE and FENCE.I
      SYSTEM -> case func3 of
        0 -> case System12 func12 of
          ECALL -> True
          EBREAK -> True
          MRET -> True
          _ -> False
        _ -> case unpack (slice d1 d0 func3) of
          ReadWrite -> True
          ReadSet -> True
          ReadClear -> True
          _ -> False
      _ -> False
  }
 where
  opcode = unpack (slice d6 d0 w)
  func3  = slice d14 d12 w
  func7  = slice d31 d25 w
  func12 = slice d31 d20 w

type Legality = Bool
legal' :: Legality
legal' = True
illegal' :: Legality
illegal' = False

{-# INLINE partialCompressedInstruction #-}
partialCompressedInstruction :: String -> Legality -> DecodedInstruction
partialCompressedInstruction instr legality = DecodedInstruction
  { opcode = getOpcode instr
  , rd     = X0
  , rs1    = X0
  , rs2    = X0
  , iop    = getIop instr
  , srla   = errorX "srla undefined"
  , shamt  = errorX "shamt undefined"
  , isSub  = False
  , isM    = False
  , mop    = errorX "mop undefined"
  , imm20U = errorX "imm20U undefined"
  , imm20J = errorX "imm20J undefined"
  , imm12I = errorX "imm12I undefined"
  , imm12S = errorX "imm12S undefined"
  , imm12B = errorX "imm12B undefined"
  , func3  = getFunc3 instr
  , compressed = True
  , legal  = legality
  }

data DecodedCompressedInstruction = DecodedCompressedInstruction {
    cOpcode      :: BitVector 2
  , cFunct2      :: BitVector 2
  , cFunct2'     :: BitVector 2
  , cFunct3      :: BitVector 3
  , cFunct4      :: BitVector 4
  , cRd          :: BitVector 5
  , cRs1         :: BitVector 5
  , cRs2         :: BitVector 5
  , cRd'         :: BitVector 3
  , cRd''        :: BitVector 3
  , cRs1'        :: BitVector 3
  , cRs2'        :: BitVector 3
  , cImm5LS      :: BitVector 5
  , cImm6I       :: BitVector 6
  , cImm6I'      :: BitVector 6
  , cImm6I''     :: BitVector 6
  , cImm6SS      :: BitVector 6
  , cImm8IW      :: BitVector 8
  , cOffset      :: BitVector 8
  , cJumpTarget  :: BitVector 11
  , cShamt       :: BitVector 6} deriving Show


decode16 :: MachineWord -> DecodedCompressedInstruction
decode16 w = DecodedCompressedInstruction{
    cOpcode      = slice d1 d0 w
  , cFunct2      = slice d6 d5 w
  , cFunct2'     = slice d11 d10 w
  , cFunct3      = slice d15 d13 w
  , cFunct4      = slice d15 d12 w
  , cRd          = slice d11 d7 w
  , cRs1         = slice d11 d7 w
  , cRs2         = slice d6 d2 w
  , cRd'         = slice d4 d2 w
  , cRd''        = slice d9 d7 w
  , cRs1'        = slice d9 d7 w
  , cRs2'        = slice d4 d2 w
  , cImm5LS      = slice d5 d5 w ++# slice d12 d10 w ++# slice d6 d6 w
  , cImm6I       = slice d12 d12 w ++# slice d6 d2 w
  , cImm6I'      = slice d12 d12 w ++# slice d4 d3 w ++# slice d5 d5 w ++# slice d2 d2 w ++# slice d6 d6 w
  , cImm6I''     = slice d3 d2 w ++# slice d12 d12 w ++# slice d6 d4 w
  , cImm6SS      = slice d8 d7 w ++# slice d12 d9 w
  , cImm8IW      = slice d10 d7 w ++# slice d12 d11 w ++# slice d5 d5 w ++# slice d6 d6 w
  , cOffset      = slice d12 d12 w ++# slice d6 d5 w ++# slice d2 d2 w ++# slice d11 d10 w ++# slice d4 d3 w
  , cJumpTarget  = slice d12 d12 w ++# slice d8 d8 w ++# slice d10 d9 w ++# slice d6 d6 w ++# slice d7 d7 w ++# slice d2 d2 w ++# slice d11 d11 w ++# slice d5 d3 w
  , cShamt       = slice d12 d12 w ++# slice d6 d2 w
  }

compressedToFull :: DecodedCompressedInstruction -> DecodedInstruction
compressedToFull DecodedCompressedInstruction
  { cOpcode, cFunct2, cFunct2', cFunct3, cFunct4, cRd, cRs1, cRs2, cRd', cRd''
  , cRs1', cRs2', cImm5LS, cImm6I, cImm6I', cImm6I'', cImm6SS, cImm8IW, cOffset
  , cJumpTarget , cShamt
  } = case (cFunct3, cFunct4, cFunct2', cRd, cFunct2, cRs2, cOpcode) of
  (0b000, _       , _, _, _, _, 0b00) -> (pci "ADDI"  legal'    ){imm12I = zeroExtend cImm8IW * 4, rs1 = X2, rd = rd'}
  (0b010, _       , _, _, _, _, 0b00) -> (pci "LW"    legal'    ){imm12I = shiftL (zeroExtend cImm5LS) 2, rs1 = rs1', rd = rd'}
  (0b110, _       , _, _, _, _, 0b00) -> (pci "SW"    legal'    ){imm12S = shiftL (zeroExtend cImm5LS) 2 , rs1 = rs1', rs2 = rs2', rd = rd'}
  (0b000, _       , _, _, _, _, 0b01) -> (pci "ADDI"  legal'    ){isSub = False, imm12I = signExtend cImm6I, rs1 = unpack cRs1, rd = unpack cRd}
  (0b001, _       , _, _, _, _, 0b01) -> (pci "JAL"   legal'    ){rd = X1, imm20J = signExtend cJumpTarget}
  (0b010, _       , _, _, _, _, 0b01) -> (pci "ADDI"  legal'    ){isSub = False, imm12I = signExtend cImm6I, rs1 = X0, rd = unpack cRd}
  (0b011, _       , _, 2, _, _, 0b01) -> (pci "ADDI"  legal'    ){isSub = False, imm12I = shiftL (signExtend cImm6I') 4, rs1 = X2, rd = X2}
  (0b011, _       , _, _, _, _, 0b01) -> (pci "LUI"   luiLegal  ){imm20U = signExtend cImm6I, rs1 = X0, rd = unpack cRd}
  (0b100, _       , 0, _, _, _, 0b01) -> (pci "SRLI"  shiftLegal){srla = Logical, rd = rd'', rs1 = rs1', imm12I = zeroExtend $ slice d4 d0 cShamt}
  (0b100, _       , 1, _, _, _, 0b01) -> (pci "SRAI"  shiftLegal){srla = Arithmetic, rd = rd'', rs1 = rs1', imm12I = zeroExtend $ slice d4 d0 cShamt}
  (0b100, _       , 2, _, _, _, 0b01) -> (pci "ANDI"  legal'    ){rd = rd'', rs1 = rs1', imm12I = signExtend cImm6I}
  (_    , 0b1000  , 3, _, 0, _, 0b01) -> (pci "SUB"   legal'    ){isSub = True, rd = rd'', rs1 = rs1', rs2 = rs2', imm12I = signExtend cImm6I}
  (_    , 0b1000  , 3, _, 1, _, 0b01) -> (pci "XOR"   legal'    ){rd = rd'', rs1 = rs1', rs2 = rs2', imm12I = signExtend cImm6I}
  (_    , 0b1000  , 3, _, 2, _, 0b01) -> (pci "OR"    legal'    ){rd = rd'', rs1 = rs1', rs2 = rs2', imm12I = signExtend cImm6I}
  (_    , 0b1000  , 3, _, 3, _, 0b01) -> (pci "AND"   legal'    ){rd = rd'', rs1 = rs1', rs2 = rs2', imm12I = signExtend cImm6I}
  (0b101, _       , _, _, _, _, 0b01) -> (pci "JAL"   legal'    ){imm20J = signExtend cJumpTarget, rd = X0}
  (0b110, _       , _, _, _, _, 0b01) -> (pci "BEQ"   legal'    ){imm12B = signExtend cOffset, rs1 = rs1', rs2 = X0}
  (0b111, _       , _, _, _, _, 0b01) -> (pci "BNE"   legal'    ){imm12B = signExtend cOffset, rs1 = rs1', rs2 = X0}
  (0b000, _       , _, _, _, _, 0b10) -> (pci "SLLI"  shiftLegal){srla = Logical, imm12I = zeroExtend cShamt, rd = unpack cRd, rs1 = unpack cRs1}
  (0b010, _       , _, _, _, _, 0b10) -> (pci "LW"    legal'    ){rd = unpack cRd, rs1 = X2, imm12I = shiftL (zeroExtend cImm6I'') 2}
  (_    , 0b1000  , _, _, _, 0, 0b10) -> (pci "JALR"  jalrLegal ){rd = X0, rs1 = unpack cRs1, imm12I = 0}
  (_    , 0b1000  , _, _, _, _, 0b10) -> (pci "ADD"   legal'    ){isSub = False, rd = unpack cRd, rs1 = X0, rs2 = unpack cRs2}
  (_    , 0b1001  , _, 0, _, 0, 0b10) -> (decode32 (1048691 :: MachineWord)){compressed = True} -- 00000000000100000000000001110011
  (_    , 0b1001  , _, _, _, 0, 0b10) -> (pci "JALR"  jalrLegal ){rd = X1, rs1 = unpack cRs1, imm12I = 0}
  (_    , 0b1001  , _, _, _, _, 0b10) -> (pci "ADD"   (cRs2/=0) ){isSub = False, rd = unpack cRd, rs1 = unpack cRs1, rs2 = unpack cRs2}
  (0b110, _       , _, _, _, _, 0b10) -> (pci "SW"    legal'    ){rs1 = X2, rs2 = unpack cRs2, imm12S = shiftL (zeroExtend cImm6SS) 2}
  (_    , _       , _, _, _, _, _   ) -> pci "__ILLEGAL__" False
 where
  pci  = partialCompressedInstruction
  rs1' = decompressReg cRs1'
  rs2' = decompressReg cRs2'
  rd'  = decompressReg cRd'
  rd'' = decompressReg cRd''
  shiftLegal = slice d5 d5 cShamt == 0
  jalrLegal = cRs1 /= 0
  luiLegal = not (cRd == 2 && cRd == 0)

getIop :: String -> IOp
getIop = \case
  "ADD"   -> ADD
  "ADDI"  -> ADD
  "SRLI"  -> SR
  "SRAI"  -> SR
  "AND"   -> AND
  "ANDI"  -> AND
  "SUB"   -> ADD
  "XOR"   -> XOR
  "OR"    -> OR
  "SLLI"  -> SLL
  _       -> ADD -- default


decompressReg :: BitVector 3 -> Register
decompressReg n = unpack $ zeroExtend n + 8

{-# INLINE getOpcode #-}
getOpcode :: String -> Opcode
getOpcode = \case
  "LUI"    -> LUI
  "AUIPC"  -> AUIPC
  "JAL"    -> JAL
  "JALR"   -> JALR
  "BEQ"    -> BRANCH
  "BNE"    -> BRANCH
  "BLT"    -> BRANCH
  "BGE"    -> BRANCH
  "BLTU"   -> BRANCH
  "BGEU"   -> BRANCH
  "LB"     -> LOAD
  "LH"     -> LOAD
  "LW"     -> LOAD
  "LBU"    -> LOAD
  "LHU"    -> LOAD
  "SB"     -> STORE
  "SH"     -> STORE
  "SW"     -> STORE
  "ADDI"   -> OP_IMM
  "SLTI"   -> OP_IMM
  "SLTIU"  -> OP_IMM
  "XORI"   -> OP_IMM
  "ORI"    -> OP_IMM
  "ANDI"   -> OP_IMM
  "SLLI"   -> OP_IMM
  "SRLI"   -> OP_IMM
  "SRAI"   -> OP_IMM
  "ADD"    -> OP
  "SUB"    -> OP
  "SLL"    -> OP
  "SLT"    -> OP
  "SLTU"   -> OP
  "XOR"    -> OP
  "SRL"    -> OP
  "SRA"    -> OP
  "OR"     -> OP
  "AND"    -> OP
  "FENCE"  -> MISC_MEM
  "ECALL"  -> SYSTEM
  "EBREAK" -> SYSTEM
  "MUL"    -> OP
  "MULH"   -> OP
  "MULHSU" -> OP
  "MULHU"  -> OP
  "DIV"    -> OP
  "DIVU"   -> OP
  "REM"    -> OP
  "REMU"   -> OP
  "__ILLEGAL__" -> errorX "Illegal instruction opcode."
  txt      -> error $ "Opcode is not known for " <> txt

{-# INLINE getFunc3 #-}
getFunc3 :: String -> BitVector 3
getFunc3 = \case

  "AUIPC"  -> errorX "AUIPC func3 undefined"
  "JAL"    -> errorX "JAL func3 undefined"
  "JALR"   -> 0b000
  "BEQ"    -> 0b000
  "BNE"    -> 0b001
  "BLT"    -> 0b100
  "BGE"    -> 0b101
  "BLTU"   -> 0b110
  "BGEU"   -> 0b111
  "LB"     -> 0b000
  "LH"     -> 0b001
  "LW"     -> 0b010
  "LBU"    -> 0b100
  "LHU"    -> 0b101
  "SB"     -> 0b000
  "SH"     -> 0b001
  "SW"     -> 0b010
  "ADDI"   -> 0b000
  "SLTI"   -> 0b010
  "SLTIU"  -> 0b011
  "XORI"   -> 0b100
  "ORI"    -> 0b110
  "ANDI"   -> 0b111
  "SLLI"   -> 0b001
  "SRLI"   -> 0b101
  "SRAI"   -> 0b101
  "ADD"    -> 0b000
  "SUB"    -> 0b000
  "SLL"    -> 0b001
  "SLT"    -> 0b010
  "SLTU"   -> 0b011
  "XOR"    -> 0b100
  "SRL"    -> 0b101
  "SRA"    -> 0b101
  "OR"     -> 0b110
  "AND"    -> 0b111
  "FENCE"  -> 0b000
  "ECALL"  -> 0b000
  "EBREAK" -> 0b000
  "MUL"    -> 0b000
  "MULH"   -> 0b001
  "MULHSU" -> 0b010
  "MULHU"  -> 0b011
  "DIV"    -> 0b100
  "DIVU"   -> 0b101
  "REM"    -> 0b110
  "REMU"   -> 0b111
  "__ILLEGAL__" -> errorX "Illegal instruction func3."
  txt      -> error $ "Func3 could is not known for " <> txt
