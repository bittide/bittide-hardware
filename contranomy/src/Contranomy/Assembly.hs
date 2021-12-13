module Contranomy.Assembly where
import Clash.Prelude

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
  | MULH    Rd Rs1 Rs2
  | MULHSU  Rd Rs1 Rs2
  | MULHU   Rd Rs1 Rs2
  | DIV     Rd Rs1 Rs2
  | DIVU    Rd Rs1 Rs2
  | REM     Rd Rs1 Rs2
  | REMU    Rd Rs1 Rs2
  deriving Show

data CompressedInstruction =
    CDDI4SPN
  | CFLD
  | CLW
  | CFLW
  | CFSD
  | CSW
  | CFSW
  | CNOP
  | CADDI
  | CJAL
  | CLI
  | CADDI16SP
  | CLUI
  | CSRLI
  | CSRAI
  | CANDI
  | CSUB
  | CXOR
  | COR
  | CAND
  | CJ
  | CBEQZ
  | CBNEZ
  | CSLLI
  | CFLDSP
  | CLWSP
  | CFLWSP
  | CJR
  | CMV
  | CEBREAK
  | CJALR
  | CADD
  | CFSDSP
  | CSWSP
  | CFSWSP
  | CSDSP
  deriving Show

data Encoding = 
   RType Opcode Rd Rs1 Rs2 Funct3 Funct7
 | IType Opcode Rd Rs1 Funct3 Imm12
 | SType Opcode Rs1 Rs2 Funct3 Imm12
 | BType Opcode Rs1 Rs2 Funct3 Imm13
 | UType Opcode Rd Imm21U
 | JType Opcode Rd Imm21L

instructionToMachineword = formatToMachineWord . instructionToFormat

formatToMachineWord (RType opcode rd rs1 rs2 funct3 funct7) = funct7 ++# rs2 ++# rs1 ++# funct3 ++# rd ++# opcode
formatToMachineWord (IType opcode rd rs1 funct3 imm) = imm ++# rs1 ++# funct3 ++# rd ++# opcode
formatToMachineWord (SType opcode rs1 rs2 funct3 imm) = (slice d11 d5 imm) ++# rs2 ++# rs1 ++# funct3 ++# (slice d4 d0 imm) ++# opcode
formatToMachineWord (BType opcode rs1 rs2 funct3 imm) = (slice d12 d12 imm) ++# (slice d10 d5 imm) ++# rs2 ++# rs1 ++# funct3 ++# (slice d4 d1 imm) ++# (slice d11 d11 imm) ++# opcode
formatToMachineWord (UType opcode rd imm) = (slice d31 d12 imm) ++# rd ++# opcode
formatToMachineWord (JType opcode rd imm) = (slice d20 d20 imm) ++# (slice d10 d1 imm) ++# (slice d11 d11 imm) ++# (slice d19 d12 imm) ++# rd ++# opcode

instructionToFormat (LUI     rd imm)              = UType 0b0110111 rd imm
instructionToFormat (AUIPC   rd imm)              = UType 0b0010111 rd imm
instructionToFormat (JAL     rd imm)              = JType 0b1100111 rd imm
instructionToFormat (JALR    rd rs1 imm)          = IType 0b1101111 rd rs1 0b000 imm
instructionToFormat (BEQ     rs1 rs2 imm)         = BType 0b1100011 rs1 rs2 0b000 imm
instructionToFormat (BNE     rs1 rs2 imm)         = BType 0b1100011 rs1 rs2 0b001 imm
instructionToFormat (BLT     rs1 rs2 imm)         = BType 0b1100011 rs1 rs2 0b100 imm
instructionToFormat (BGE     rs1 rs2 imm)         = BType 0b1100011 rs1 rs2 0b101 imm
instructionToFormat (BLTU    rs1 rs2 imm)         = BType 0b1100011 rs1 rs2 0b110 imm
instructionToFormat (BGEU    rs1 rs2 imm)         = BType 0b1100011 rs1 rs2 0b111 imm
instructionToFormat (LB      rd rs1 imm)          = IType 0b0000011 rd rs1 0b000 imm
instructionToFormat (LH      rd rs1 imm)          = IType 0b0000011 rd rs1 0b001 imm
instructionToFormat (LW      rd rs1 imm)          = IType 0b0000011 rd rs1 0b010 imm
instructionToFormat (LBU     rd rs1 imm)          = IType 0b0000011 rd rs1 0b100 imm
instructionToFormat (LHU     rd rs1 imm)          = IType 0b0000011 rd rs1 0b101 imm
instructionToFormat (SB      rs1 rs2 imm)         = SType 0b0100011 rs1 rs2 0b000 imm
instructionToFormat (SH      rs1 rs2 imm)         = SType 0b0100011 rs1 rs2 0b001 imm
instructionToFormat (SW      rs1 rs2 imm)         = SType 0b0100011 rs1 rs2 0b010 imm
instructionToFormat (ADDI    rd rs1 imm)          = IType 0b0010011 rd rs1 0b000 imm
instructionToFormat (SLTI    rd rs1 imm)          = IType 0b0010011 rd rs1 0b010 imm
instructionToFormat (SLTIU   rd rs1 imm)          = IType 0b0010011 rd rs1 0b011 imm
instructionToFormat (XORI    rd rs1 imm)          = IType 0b0010011 rd rs1 0b100 imm
instructionToFormat (ORI     rd rs1 imm)          = IType 0b0010011 rd rs1 0b110 imm
instructionToFormat (ANDI    rd rs1 imm)          = IType 0b0010011 rd rs1 0b111 imm
instructionToFormat (SLLI    rd rs1 shamt)        = RType 0b0010011 rd rs1 shamt 0b001 0b0000000
instructionToFormat (SRLI    rd rs1 shamt)        = RType 0b0010011 rd rs1 shamt 0b101 0b0000000
instructionToFormat (SRAI    rd rs1 shamt)        = RType 0b0010011 rd rs1 shamt 0b101 0b0100000
instructionToFormat (ADD     rd rs1 rs2)          = RType 0b0110011 rd rs1 rs2 0b000 0b0000000
instructionToFormat (SUB     rd rs1 rs2)          = RType 0b0110011 rd rs1 rs2 0b000 0b0100000
instructionToFormat (SLL     rd rs1 rs2)          = RType 0b0110011 rd rs1 rs2 0b001 0b0000000
instructionToFormat (SLT     rd rs1 rs2)          = RType 0b0110011 rd rs1 rs2 0b010 0b0000000
instructionToFormat (SLTU    rd rs1 rs2)          = RType 0b0110011 rd rs1 rs2 0b011 0b0000000
instructionToFormat (XOR     rd rs1 rs2)          = RType 0b0110011 rd rs1 rs2 0b100 0b0000000
instructionToFormat (SRL     rd rs1 rs2)          = RType 0b0110011 rd rs1 rs2 0b101 0b0000000
instructionToFormat (SRA     rd rs1 rs2)          = RType 0b0110011 rd rs1 rs2 0b101 0b0100000
instructionToFormat (OR      rd rs1 rs2)          = RType 0b0110011 rd rs1 rs2 0b110 0b0000000
instructionToFormat (AND     rd rs1 rs2)          = RType 0b0110011 rd rs1 rs2 0b111 0b0000000
instructionToFormat (FENCE   rd rs1 pred succ fm) = IType 0b0001111 rd rs1 0 (fm ++# pred ++# succ)
instructionToFormat (ECALL)                       = IType 0b1110011 0 0 0 0
instructionToFormat (EBREAK)                      = IType 0b1110011 0 0 0 1
instructionToFormat (MUL     rd rs1 rs2)          = RType 0b0110011 rd rs1 rs2 0b000 0b0000001
instructionToFormat (MULH    rd rs1 rs2)          = RType 0b0110011 rd rs1 rs2 0b001 0b0000001
instructionToFormat (MULHSU  rd rs1 rs2)          = RType 0b0110011 rd rs1 rs2 0b010 0b0000001
instructionToFormat (MULHU   rd rs1 rs2)          = RType 0b0110011 rd rs1 rs2 0b011 0b0000001
instructionToFormat (DIV     rd rs1 rs2)          = RType 0b0110011 rd rs1 rs2 0b100 0b0000001
instructionToFormat (DIVU    rd rs1 rs2)          = RType 0b0110011 rd rs1 rs2 0b101 0b0000001
instructionToFormat (REM     rd rs1 rs2)          = RType 0b0110011 rd rs1 rs2 0b110 0b0000001
instructionToFormat (REMU    rd rs1 rs2)          = RType 0b0110011 rd rs1 rs2 0b111 0b0000001

fibonacci =
  [ ADDI 0 0 0
  , ADDI 1 0 7
  , ADDI 2 0 1
  , ADD  4 2 3
  , ADDI 2 3 0
  , ADDI 3 4 0
  , ADDI 1 1 (-1)
  , BGE  1 0 (-16)
  , SW 0 1 0
  , SW 0 2 4
  , SW 0 3 8
  , SW 0 4 12
  , DIV 4 4 2
  , SW 0 4 16
  , ADDI 0 0 0
  ]
