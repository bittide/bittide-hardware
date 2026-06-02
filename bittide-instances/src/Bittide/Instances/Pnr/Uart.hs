-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Pnr.Uart where

import Clash.Prelude
import Protocols

import Bittide.Instances.Domains (Basic350)
import Bittide.Instances.Hacks (reducePins)
import Bittide.SharedTypes (Byte, withLittleEndian)
import Clash.Cores.Xilinx.DcFifo (dcFifoDf)
import Data.Char (ord)
import Protocols.Experimental.Wishbone
import Protocols.Extra (FunctorC (fmapC))
import Protocols.MemoryMap

import qualified Bittide.Df as Df
import qualified Bittide.Wishbone as Wb

type Baud = 921_600

baud :: SNat Baud
baud = SNat

uartLabels :: Vec 3 (Vec 2 Byte)
uartLabels =
  fmap (fromIntegral . ord)
    <$> ( $(listToVecTH "CC")
            :> $(listToVecTH "BB")
            :> $(listToVecTH "AA")
            :> Nil
        )

{- | Instead of testing all UART components individually we test the whole chain of components
as they are used in the demo's. This means:
- 3x 'uartInterfaceWb' with 'uartBytes' as their uart implementation
- For 2 uart streams a 'dcFifoDf'
- 'asciiDebugMux'
- 'uartDf'

Note that we do not connect any 'uartRx'. This is a limitation of using 'asciiDebugMux'.
-}
uartExample ::
  (HiddenClockResetEnable Basic350) =>
  Circuit
    ( Vec 3 (ToConstBwd Mm)
    , Vec 3 (Wishbone Basic350 'Standard 32 4)
    )
    (CSignal Basic350 Bit)
uartExample = withLittleEndian $ circuit $ \(memoryMaps, wbs) -> do
  [mmA, mmB, mmC] <- idC -< memoryMaps
  [wbA, wbB, wbC] <- idC -< wbs

  (uartBytesA0, _uartStatusA) <-
    Wb.uartInterfaceWb d16 d16 Wb.uartBytes -< ((mmA, wbA), Fwd (pure Nothing))
  (uartBytesB0, _uartStatusB) <-
    Wb.uartInterfaceWb d16 d16 Wb.uartBytes -< ((mmB, wbB), Fwd (pure Nothing))
  (uartBytesC, _uartStatusC) <-
    Wb.uartInterfaceWb d16 d16 Wb.uartBytes -< ((mmC, wbC), Fwd (pure Nothing))

  (uartBytesA1, uartBytesB1) <-
    fmapC
      $ dcFifoDf d5 hasClock hasReset hasClock hasReset
      -< (uartBytesA0, uartBytesB0)

  uartTxBytes <- Df.asciiDebugMux d1024 uartLabels -< [uartBytesC, uartBytesB1, uartBytesA1]

  (_uartInBytes, uartTx) <- Wb.uartDf baud -< (uartTxBytes, Fwd 0)

  idC -< uartTx

uartExampleFast :: Clock Basic350 -> Reset Basic350 -> Signal Basic350 Bit -> Signal Basic350 Bit
uartExampleFast clk rst = withClock clk $ reducePins dut
 where
  dut (unbundle -> wbIns) = bundle (bundle wbOuts, uartTx)
   where
    ((_mms, wbOuts), uartTx) =
      toSignals
        ((withClockResetEnable clk rst enableGen $ uartExample))
        ((units, wbIns), units)
