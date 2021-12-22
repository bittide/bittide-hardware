{-|
Copyright  :  (C) 2020, Christiaan Baaij
              (C) 2021, Google LLC
License    :  Apache-2.0
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
module Contranomy.Core.Decode where

import Clash.Prelude
import Contranomy.Instruction
import Contranomy.Core.SharedTypes
import qualified Data.List as L
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
  } deriving Show

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
  , isM    = pack opcode ++# slice d25 d25 w == 0b01100111
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

undefinedInstruction :: DecodedInstruction
undefinedInstruction = DecodedInstruction
  { opcode = undefined
  , rd     = X0
  , rs1    = X0
  , rs2    = X0
  , iop    = ADD
  , srla   = undefined
  , shamt  = undefined
  , isSub  = False
  , isM    = False
  , mop    = undefined
  , imm20U = undefined
  , imm20J = undefined
  , imm12I = undefined
  , imm12S = undefined
  , imm12B = undefined
  , func3  = undefined
  , compressed = undefined
  , legal  = False
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
compressedToFull DecodedCompressedInstruction {
    cOpcode
  , cFunct2
  , cFunct2'
  , cFunct3
  , cFunct4
  , cRd
  , cRs1
  , cRs2
  , cRd'
  , cRd''
  , cRs1'
  , cRs2'
  , cImm5LS
  , cImm6I
  , cImm6I'
  , cImm6I''
  , cImm6SS
  , cImm8IW
  , cOffset
  , cJumpTarget
  , cShamt     } = case (cFunct3, cFunct4, cFunct2', cRd, cFunct2, cRs2, cOpcode) of
  (0b000, _       , _, _, _, _, 0b00) -> undefinedInstruction{compressed = True, iop = ADD,  opcode = getOpcode "ADDI"   , imm12I = zeroExtend cImm8IW * 4, rs1 = X2, rd = decompressReg cRd' , func3 = getFunc3 "ADDI", legal = True}
  (0b010, _       , _, _, _, _, 0b00) -> undefinedInstruction{compressed = True, opcode = getOpcode "LW"     , imm12I = shiftL (zeroExtend cImm5LS) 2, rs1 = decompressReg cRs1', rd = decompressReg cRd', func3 = getFunc3 "LW", legal = True}
  (0b110, _       , _, _, _, _, 0b00) -> undefinedInstruction{compressed = True, opcode = getOpcode "SW"     , imm12S = shiftL (zeroExtend cImm5LS) 2 , rs1 = decompressReg cRs1', rs2 = decompressReg cRs2', rd = decompressReg cRd', func3 = getFunc3 "SW", legal = True}
  (0b000, _       , _, _, _, _, 0b01) -> undefinedInstruction{compressed = True, iop = ADD, opcode = getOpcode "ADDI", isSub = False   , imm12I = signExtend cImm6I, rs1 = unpack cRs1, rd = unpack cRd, func3 = getFunc3 "ADDI", legal = True}
  (0b001, _       , _, _, _, _, 0b01) -> undefinedInstruction{compressed = True, opcode = getOpcode "JAL"    , rd = X1, imm20J = signExtend cJumpTarget, func3 = getFunc3 "JAL", legal = True}
  (0b010, _       , _, _, _, _, 0b01) -> undefinedInstruction{compressed = True, iop = ADD, opcode = getOpcode "ADDI", isSub = False   , imm12I = signExtend cImm6I, rs1 = X0, rd = unpack cRd, func3 = getFunc3 "ADDI", legal = True}
  (0b011, _       , _, 2, _, _, 0b01) -> undefinedInstruction{compressed = True, iop = ADD, opcode = getOpcode "ADDI", isSub = False   , imm12I = shiftL (signExtend cImm6I') 4, rs1 = X2, rd = X2, func3 = getFunc3 "ADDI", legal = True}
  (0b011, _       , _, _, _, _, 0b01) -> undefinedInstruction{compressed = True, opcode = getOpcode "LUI", imm20U = signExtend cImm6I, rs1 = X0, rd = unpack cRd, func3 = getFunc3 "LUI", legal = not $ cRd == 2 && cRd == 0}
  (0b100, _       , 0, _, _, _, 0b01) -> undefinedInstruction{compressed = True, iop = SR, srla = Logical, opcode = getOpcode "SRLI"   , rd = decompressReg cRd'', rs1 = decompressReg cRs1', imm12I = zeroExtend $ slice d4 d0 cShamt, func3 = getFunc3 "SRLI", legal = slice d5 d5 cShamt == 0}
  (0b100, _       , 1, _, _, _, 0b01) -> undefinedInstruction{compressed = True, iop = SR, srla = Arithmetic, opcode = getOpcode "SRAI"   , rd = decompressReg cRd'', rs1 = decompressReg cRs1', imm12I = zeroExtend $ slice d4 d0 cShamt, func3 = getFunc3 "SRAI", legal = slice d5 d5 cShamt == 0}
  (0b100, _       , 2, _, _, _, 0b01) -> undefinedInstruction{compressed = True, iop = AND, opcode = getOpcode "ANDI"   , rd = decompressReg cRd'', rs1 = decompressReg cRs1', imm12I = signExtend cImm6I, func3 = getFunc3 "ANDI", legal = True}
  (_    , 0b1000  , 3, _, 0, _, 0b01) -> undefinedInstruction{compressed = True, opcode = getOpcode "SUB", isSub = True    , rd = decompressReg cRd'', rs1 = decompressReg cRs1', rs2 = decompressReg cRs2', imm12I = signExtend cImm6I, func3 = getFunc3 "SUB", legal = True}
  (_    , 0b1000  , 3, _, 1, _, 0b01) -> undefinedInstruction{compressed = True, iop = XOR, opcode = getOpcode "XOR"    , rd = decompressReg cRd'', rs1 = decompressReg cRs1', rs2 = decompressReg cRs2', imm12I = signExtend cImm6I, func3 = getFunc3 "XOR", legal = True}
  (_    , 0b1000  , 3, _, 2, _, 0b01) -> undefinedInstruction{compressed = True, iop = OR, opcode = getOpcode "OR"     , rd = decompressReg cRd'', rs1 = decompressReg cRs1', rs2 = decompressReg cRs2', imm12I = signExtend cImm6I, func3 = getFunc3 "OR", legal = True}
  (_    , 0b1000  , 3, _, 3, _, 0b01) -> undefinedInstruction{compressed = True, iop = AND, opcode = getOpcode "AND"    , rd = decompressReg cRd'', rs1 = decompressReg cRs1', rs2 = decompressReg cRs2', imm12I = signExtend cImm6I, func3 = getFunc3 "AND", legal = True}
  (0b101, _       , _, _, _, _, 0b01) -> undefinedInstruction{compressed = True, opcode = getOpcode "JAL"    , imm20J = signExtend cJumpTarget, rd = X0 , legal = True}
  (0b110, _       , _, _, _, _, 0b01) -> undefinedInstruction{compressed = True, opcode = getOpcode "BEQ"    , imm12B = signExtend cOffset, rs1 = decompressReg cRs1', rs2 = X0 , func3 = getFunc3 "BEQ", legal = True}
  (0b111, _       , _, _, _, _, 0b01) -> undefinedInstruction{compressed = True, opcode = getOpcode "BNE"    , imm12B = signExtend cOffset, rs1 = decompressReg cRs1', rs2 = X0 , func3 = getFunc3 "BNE", legal = True}
  (0b000, _       , _, _, _, _, 0b10) -> undefinedInstruction{compressed = True, iop = SLL, srla = Logical, opcode = getOpcode "SLLI"   , imm12I = zeroExtend cShamt, rd = unpack cRd, rs1 = unpack cRs1 , func3 = getFunc3 "SLLI", legal = slice d5 d5 cShamt == 0}
  (0b010, _       , _, _, _, _, 0b10) -> undefinedInstruction{compressed = True, opcode = getOpcode "LW"     , rd = unpack cRd, rs1 = X2, imm12I = shiftL (zeroExtend cImm6I'') 2, func3 = getFunc3 "LW", legal = True}
  (_    , 0b1000  , _, _, _, 0, 0b10) -> undefinedInstruction{compressed = True, opcode = getOpcode "JALR"   , rd = X0, rs1 = unpack cRs1, imm12I = 0, func3 = getFunc3 "JALR", legal = not $ cRs1 == 0}
  (_    , 0b1000  , _, _, _, _, 0b10) -> undefinedInstruction{compressed = True, iop = ADD, opcode = getOpcode "ADD", isSub = False    , rd = unpack cRd, rs1 = X0, rs2 = unpack cRs2, func3 = getFunc3 "ADD", legal = True}
  (_    , 0b1001  , _, 0, _, 0, 0b10) -> (decode32 (1048691 :: MachineWord)){compressed = True} -- 00000000000100000000000001110011
  (_    , 0b1001  , _, _, _, 0, 0b10) -> undefinedInstruction{compressed = True, opcode = getOpcode "JALR"   , rd = X1, rs1 = unpack cRs1, imm12I = 0, func3 = getFunc3 "JALR", legal = not $ cRs1 == 0}
  (_    , 0b1001  , _, _, _, _, 0b10) -> undefinedInstruction{compressed = True, iop = ADD, opcode = getOpcode "ADD", isSub = False    , rd = unpack cRd, rs1 = unpack cRs1, rs2 = unpack cRs2, func3 = getFunc3 "ADD", legal = not $ cRs2 == 0}
  (0b110, _       , _, _, _, _, 0b10) -> undefinedInstruction{compressed = True, opcode = getOpcode "SW" , rs1 = X2, rs2 = unpack cRs2, imm12S = shiftL (zeroExtend cImm6SS) 2, func3 = getFunc3 "SW", legal = True}
  (_    , _       , _, _, _, _, _   ) -> undefinedInstruction{compressed = True, legal = False}


decompressReg :: BitVector 3 -> Register
decompressReg n = unpack $ zeroExtend n + 8
getOpcode :: String -> Opcode
getOpcode ("LUI")     = LUI
getOpcode ("AUIPC")   = AUIPC
getOpcode ("JAL")     = JAL
getOpcode ("JALR")    = JALR
getOpcode ("BEQ")     = BRANCH
getOpcode ("BNE")     = BRANCH
getOpcode ("BLT")     = BRANCH
getOpcode ("BGE")     = BRANCH
getOpcode ("BLTU")    = BRANCH
getOpcode ("BGEU")    = BRANCH
getOpcode ("LB")      = LOAD
getOpcode ("LH")      = LOAD
getOpcode ("LW")      = LOAD
getOpcode ("LBU")     = LOAD
getOpcode ("LHU")     = LOAD
getOpcode ("SB")      = STORE
getOpcode ("SH")      = STORE
getOpcode ("SW")      = STORE
getOpcode ("ADDI")    = OP_IMM
getOpcode ("SLTI")    = OP_IMM
getOpcode ("SLTIU")   = OP_IMM
getOpcode ("XORI")    = OP_IMM
getOpcode ("ORI")     = OP_IMM
getOpcode ("ANDI")    = OP_IMM
getOpcode ("SLLI")    = OP_IMM
getOpcode ("SRLI")    = OP_IMM
getOpcode ("SRAI")    = OP_IMM
getOpcode ("ADD")     = OP
getOpcode ("SUB")     = OP
getOpcode ("SLL")     = OP
getOpcode ("SLT")     = OP
getOpcode ("SLTU")    = OP
getOpcode ("XOR")     = OP
getOpcode ("SRL")     = OP
getOpcode ("SRA")     = OP
getOpcode ("OR")      = OP
getOpcode ("AND")     = OP
getOpcode ("FENCE")   = MISC_MEM
getOpcode ("ECALL")   = SYSTEM
getOpcode ("EBREAK")  = SYSTEM
getOpcode ("MUL")     = OP
getOpcode ("MULH")    = OP
getOpcode ("MULHSU")  = OP
getOpcode ("MULHU")   = OP
getOpcode ("DIV")     = OP
getOpcode ("DIVU")    = OP
getOpcode ("REM")     = OP
getOpcode ("REMU")    = OP
getOpcode txt         = error $ "Opcode is not known for " L.++ txt

getFunc3 :: String -> BitVector 3
getFunc3 ("AUIPC" ) = undefined
getFunc3 ("JAL"   ) = undefined
getFunc3 ("JALR"  ) = 0b000
getFunc3 ("BEQ"   ) = 0b000
getFunc3 ("BNE"   ) = 0b001
getFunc3 ("BLT"   ) = 0b100
getFunc3 ("BGE"   ) = 0b101
getFunc3 ("BLTU"  ) = 0b110
getFunc3 ("BGEU"  ) = 0b111
getFunc3 ("LB"    ) = 0b000
getFunc3 ("LH"    ) = 0b001
getFunc3 ("LW"    ) = 0b010
getFunc3 ("LBU"   ) = 0b100
getFunc3 ("LHU"   ) = 0b101
getFunc3 ("SB"    ) = 0b000
getFunc3 ("SH"    ) = 0b001
getFunc3 ("SW"    ) = 0b010
getFunc3 ("ADDI"  ) = 0b000
getFunc3 ("SLTI"  ) = 0b010
getFunc3 ("SLTIU" ) = 0b011
getFunc3 ("XORI"  ) = 0b100
getFunc3 ("ORI"   ) = 0b110
getFunc3 ("ANDI"  ) = 0b111
getFunc3 ("SLLI"  ) = 0b001
getFunc3 ("SRLI"  ) = 0b101
getFunc3 ("SRAI"  ) = 0b101
getFunc3 ("ADD"   ) = 0b000
getFunc3 ("SUB"   ) = 0b000
getFunc3 ("SLL"   ) = 0b001
getFunc3 ("SLT"   ) = 0b010
getFunc3 ("SLTU"  ) = 0b011
getFunc3 ("XOR"   ) = 0b100
getFunc3 ("SRL"   ) = 0b101
getFunc3 ("SRA"   ) = 0b101
getFunc3 ("OR"    ) = 0b110
getFunc3 ("AND"   ) = 0b111
getFunc3 ("FENCE" ) = 0b000
getFunc3 ("ECALL" ) = 0b000
getFunc3 ("EBREAK") = 0b000
getFunc3 ("MUL"   ) = 0b000
getFunc3 ("MULH"  ) = 0b001
getFunc3 ("MULHSU") = 0b010
getFunc3 ("MULHU" ) = 0b011
getFunc3 ("DIV"   ) = 0b100
getFunc3 ("DIVU"  ) = 0b101
getFunc3 ("REM"   ) = 0b110
getFunc3 ("REMU"  ) = 0b111
getFunc3 txt       = error $ "Func3 could is not known for " L.++ txt
