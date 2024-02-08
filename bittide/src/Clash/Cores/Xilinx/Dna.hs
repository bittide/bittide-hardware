-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Clash.Cores.Xilinx.Dna where

import Clash.Explicit.Prelude hiding (shift, read)
import Clash.Prelude (HiddenClockResetEnable, hasClock, hasReset, hasEnable)

import Clash.Annotations.Primitive
import Clash.Signal (withClockResetEnable)
import Data.String.Interpolate      (__i)
import Clash.Annotations.TH (makeTopEntity)

data State
  = Load
  | Shift
  | Done
  deriving (Generic, NFDataX)

type DIN = Bit
type DOUT = Bit
type READ = Bool
type SHIFT = Bool

-- | A valid DNA to be used for simulation purposes.
defaultSimDNA :: BitVector 96
defaultSimDNA = 0x4ABABAB55555555DEADBEEF1

{-# NOINLINE deviceDna #-}
deviceDna ::
  HiddenClockResetEnable dom =>
  BitVector 96 ->
  "maybeDNA" ::: Signal dom (Maybe (BitVector 96))
deviceDna simDna= mux done (Just <$> dna) (pure Nothing)
 where
  (rd, sh, dna, done) = danPorteControl hasClock hasReset hasEnable dout
  dout = dnaPorte2 simDna hasClock dout rd sh

-- | State machine to extract the DNA from Kintex Ultrascale FPGAs.
-- To be connected to the Device DNA Access Port @DNA_PORTE2@
--
-- Instantiation template:
--
-- @
--  DNA_PORTE2 #(
--     .SIM_DNA_VALUE(96'h000000000000000000000000)  // Specifies a sample 96-bit DNA value for simulation.
--  )
--  DNA_PORTE2_inst (
--     .DOUT(DOUT),   // 1-bit output: DNA output data.
--     .CLK(CLK),     // 1-bit input: Clock input.
--     .DIN(DIN),     // 1-bit input: User data input pin.
--     .READ(READ),   // 1-bit input: Active-High load DNA, active-Low read input.
--     .SHIFT(SHIFT)  // 1-bit input: Active-High shift enable input.
--  );
-- @
danPorteControl ::
  KnownDomain dom =>
  "clk" ::: Clock dom ->
  "rst" ::: Reset dom ->
  "ena" ::: Enable dom ->
  "dout" ::: Signal dom DOUT ->
  "" :::
  ( "read" ::: Signal dom READ
  , "shift" ::: Signal dom SHIFT
  , "dna" ::: Signal dom (BitVector 96)
  , "done" ::: Signal dom  Bool)

danPorteControl clk rst ena dout = (rd .&&. fromEnable ena, sh .&&. fromEnable ena, dna, done)
 where
  (rd, sh, dna, done) = mealyB clk rst ena go (Load, 0) dout
  go :: (State, BitVector 96) -> DOUT -> ((State, BitVector 96), (READ, SHIFT, BitVector 96, Bool))
  go (state,reg) input = (newState, (r, s, reg, dnaDetected))
   where
    (newState, r, s) = case state of
      Load  -> ((Shift, newShift), True, False)
      Shift
        | dnaDetected -> ((Done, reg), False, False)
        | otherwise   -> ((Shift, newShift), False, True)
      Done  -> ((Done, reg), False, False)
    newShift =  input +>>. reg
    dnaDetected = slice d95 d94 reg == 1 && slice d1 d0 reg == 1

{-# ANN dnaPorte2 (InlineYamlPrimitive [Verilog] $
  let
    bbName = show 'dnaPorte2
  in [__i|
    BlackBox:
      name: #{bbName}
      kind: Declaration
      type: |-
        dnaPorte2 ::
          KnownDomain dom =>    -- ARG[0]
          BitVector 96 ->       -- ARG[1]
          Clock dom ->          -- ARG[2]
          Signal dom DIN ->     -- ARG[3]
          Signal dom READ ->    -- ARG[4]
          Signal dom SHIFT ->   -- ARG[5]
          Signal dom DOUT       -- RESULT
      template: |-
        // Begin DNA-PORTE2 instantiation
        DNA_PORTE2 \#(
            .SIM_DNA_VALUE(~ARG[1])  // Specifies a sample 96-bit DNA value for simulation.
        )
        DNA_PORTE2_inst (
            .DOUT(~RESULT),   // 1-bit output: DNA output data.
            .CLK(~ARG[2]),     // 1-bit input: Clock input.
            .DIN(~ARG[3]),     // 1-bit input: User data input pin.
            .READ(~ARG[4]),   // 1-bit input: Active-High load DNA, active-Low read input.
            .SHIFT(~ARG[5])  // 1-bit input: Active-High shift enable input.
        );
        // End DNA-PORTE2 instantiation
|]) #-}

-- | From https://docs.xilinx.com/r/2021.2-English/ug974-vivado-ultrascale-libraries/DNA_PORTE2:
--
-- The DNA_PORT allows access to a dedicated shift register that can be loaded
-- with the Device DNA data bits (factory-programmed, read-only unique ID) for a
-- given UltraScale device. In addition to shifting out the DNA data bits, this
-- component allows for the inclusion of supplemental bits of your data, or
-- allows for the DNA data to rollover (repeat DNA data after initial data has
-- been shifted out). This component is primarily used in conjunction with other
-- circuitry to build added copy protection for the device bitstream from possible
-- theft.
--
-- To access the Device DNA data, you must first load the shift register by setting
-- the active-High READ signal for one clock cycle. After the shift register is
-- loaded, the data can be synchronously shifted out by enabling the active-High
-- SHIFT input and capturing the data out the DOUT output port. Additional data can
-- be appended to the end of the 96-bit shift register by connecting the appropriate
-- logic to the DIN port. If DNA data rollover is desired, connect the DOUT port
-- directly to the DIN port to allow for the same data to be shifted out after
-- completing the 96-bit shift operation. If no additional data is necessary, the
-- DIN port can be tied to a logic zero. The attribute SIM_DNA_VALUE can be
-- optionally set to allow for simulation of a possible DNA data sequence. By
-- default, the Device DNA data bits are all zeros in the simulation model.
dnaPorte2 ::
  KnownDomain dom =>
  -- | Simulation only DNA value, must have bits @[95:94]@ and @[1:0]@ set to @0b01@.
  BitVector 96 ->
  -- | Incoming clock signal.
  Clock dom ->
  -- | Shift register input pin.
  Signal dom DIN ->
  -- | Active high load DNA, active low read input.
  Signal dom READ ->
  -- | Active high shift enable.
  Signal dom SHIFT ->
  -- | DNA output pin.
  Signal dom DOUT
dnaPorte2 dnaSimValue clk  din read shift
  | slice d95 d94 dnaSimValue /= 0b01 || slice d1 d0 dnaSimValue /= 0b01 =
    error "dnaPorte2: Supplied simulation DNA must have bits [95:94] and [1:0] set to 0b01"
  | otherwise = unpack . resize <$> regOut
 where
  regOut = register clk resetGen enableGen 0 $
    mux read
    (pure dnaSimValue)
    (mux shift
      ((+>>.) <$> din <*> regOut)
      regOut
    )
{-# NOINLINE dnaPorte2 #-}
{-# ANN dnaPorte2 hasBlackBox #-}

deviceDnaSystem ::
  "clk" ::: Clock System ->
  "rst" ::: Reset System ->
  "maybeDNA" ::: Signal System (Maybe (BitVector 96))
deviceDnaSystem  clk rst = withClockResetEnable clk rst enableGen
  $ deviceDna @System defaultSimDNA

makeTopEntity 'deviceDnaSystem
