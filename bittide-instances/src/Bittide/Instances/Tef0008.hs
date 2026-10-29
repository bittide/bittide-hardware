-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | Pins of the Trenz TEF0008 FMC card, which holds the SFP+ cages of links J4
to J7. See https://wiki.trenz-electronic.de/display/PD/TEF0008+MAX10.
-}
module Bittide.Instances.Tef0008 (
  SfpControl,
  sfpControl,
  si5345MuxSelect,
  iicMuxResetB,
) where

import Clash.Explicit.Prelude

{- | SFP control signals, which the card's MAX10 forwards inverted to all four
SFP+ cages. Constrained by @tef0008/sfpControl.xdc@.

  * LA03_P: TX_DISABLE
  * LA03_N: RS0 (RX rate select)
  * LA04_P: RS1 (TX rate select)

These must be driven whenever VADJ is on, because VADJ powers the MAX10 I/O bank
that holds these pins. If left undriven, the FPGA's default pull-down on unused
pins wins over the MAX10's weak pull-up, which can disable the transmitters of
modules that implement TX_DISABLE.
-}
type SfpControl dom =
  ( "FMC_HPC_LA03_P" ::: Signal dom Bit
  , "FMC_HPC_LA03_N" ::: Signal dom Bit
  , "FMC_HPC_LA04_P" ::: Signal dom Bit
  )

{- | Transmitters on, and both rate selects on full rate, as our links run at
10 Gbps. Levels are given at the FMC pins. The MAX10 inverts them, so the SFP
pins see TX_DISABLE low, and RS0 and RS1 high.
-}
sfpControl :: SfpControl dom
sfpControl =
  ( pure high -- LA03_P high: SFP TX_DISABLE low, transmitters on
  , pure low -- LA03_N low: SFP RS0 high, full RX rate
  , pure low -- LA04_P low: SFP RS1 high, full TX rate
  )

{- | Selects the Si5345 on the I2C multiplexer in the MAX10 of the card, for
@FMC_MUX_SELECT@ (LA01_N, LA02_P, LA02_N). The other values (0b000 to 0b011)
select the I2C of SFP cages A to D. See the TEF0008 MAX10 documentation by Trenz
Electronic. Constrained by @tef0008/si5345I2c.xdc@.
-}
si5345MuxSelect :: Signal dom (BitVector 3)
si5345MuxSelect = pure 0b100

{- | Active-low reset of the TCA9548 I2C switch on the main I2C bus of the KCU105,
for @IIC_MUX_RESET_B_LS@. We do not use that switch: keep it out of reset, so it
does not hold up the bus. A KCU105 pin, but we only use the main I2C bus to set
VADJ for this card. Constrained by @tef0008/mainI2c.xdc@.
-}
iicMuxResetB :: Signal dom Bit
iicMuxResetB = pure high
