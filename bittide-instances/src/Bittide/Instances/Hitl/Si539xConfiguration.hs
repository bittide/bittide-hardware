-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}

{- | A hardware-in-the-loop test to verify we can program the external clock
board from software using a specialized memory-mapped SPI device.
-}
module Bittide.Instances.Hitl.Si539xConfiguration where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)

import GHC.Stack (HasCallStack)
import System.FilePath ((</>))

import Clash.Annotations.TH (makeTopEntity)
import Clash.Explicit.Reset.Extra (Asserted (..), xpmResetSynchronizer)
import Clash.Xilinx.ClockGen (clockWizardDifferential)
import Protocols
import Protocols.MemoryMap
import VexRiscv

import Bittide.ClockControl.Si539xSpi
import Bittide.Counter (domainDiffCountersWbC)
import Bittide.DoubleBufferedRam
import Bittide.Hitl (
  HitlTestCase (..),
  HitlTestGroup (..),
  HwTargetRef (HwTargetByIndex),
  hitlVioBool,
  paramForSingleHwTarget,
 )
import Bittide.ProcessingElement (PeConfig (..), processingElement)
import Bittide.SharedTypes (withBittideByteOrder)
import Bittide.Wishbone (timeWb, uartBytes, uartDf, uartInterfaceWb)
#ifdef SIM_BAUD_RATE
import Clash.Cores.UART.Extra
#endif

import Bittide.Instances.Domains

import qualified Bittide.Instances.Hitl.Driver.Si539xConfiguration as D
import qualified Clash.Cores.Xilinx.Extra as Gth

#ifdef SIM_BAUD_RATE
type Baud = MaxBaudRate Basic125
#else
type Baud = 921_600
#endif

baud :: SNat Baud
baud = SNat

si539xConfigTest ::
  "CLK_125MHZ" ::: DiffClock Ext125 ->
  "SMA_MGT_REFCLK_C" ::: DiffClock Ext200 ->
  "JTAG" ::: Signal Basic125 JtagIn ->
  "MISO" ::: Signal Basic125 Bit ->
  "USB_UART_TXD" ::: Signal Basic125 Bit ->
  ( "JTAG" ::: Signal Basic125 JtagOut
  , "USB_UART_RXD" ::: Signal Basic125 Bit
  , ""
      ::: ( "SCLK" ::: Signal Basic125 Bool
          , "MOSI" ::: Signal Basic125 Bit
          , "CSB" ::: Signal Basic125 Bool
          )
  )
si539xConfigTest freeClkDiff skyClkDiff jtagIn miso _uartRx =
  testStart `hwSeqX` (jtagOut, uartTx, spiOut)
 where
  (freeClk, freeRst) = clockWizardDifferential freeClkDiff noReset
  (_, odivClk) = Gth.ibufds_gte3 skyClkDiff
  skyClk :: Clock Basic200
  skyClk = Gth.bufgGt d0 odivClk noReset

  -- We don't need the VIO for this test, but our current HITL infrastructure always
  -- tries to generate a probes file, which it can't if there is no VIO.
  testStart = hitlVioBool freeClk testDone testSuccess
  testDone = register freeClk testRst enableGen False (pure True)
  testSuccess = register freeClk testRst enableGen False (pure True)
  testRst = unsafeFromActiveLow testStart

  ((jtagOut, _), (uartTx, spiOut)) =
    toSignals
      ( circuit $ \(jtagIn, miso) -> do
          mm <- ignoreMM
          (uartOutBytes, _spiDone, spiOut) <-
            dut freeClk freeRst skyClk -< (mm, (jtagIn, miso))

          (_uartInBytes, uartTx) <-
            withClockResetEnable freeClk freeRst enableGen
              $ uartDf baud
              -< (uartOutBytes, Fwd (pure 0))
          idC -< (uartTx, spiOut)
      )
      ((jtagIn, miso), ((), ((), (), ())))

dut ::
  forall free sky.
  ( HasCallStack
  , KnownDomain free
  , KnownDomain sky
  , HasSynchronousReset free
  , HasSynchronousReset sky
  , 1 <= DomainPeriod free
  ) =>
  Clock free ->
  Reset free ->
  Clock sky ->
  Circuit
    ( "MM" ::: ConstBwd MM
    , ( Jtag free
      , "MISO" ::: CSignal free Bit
      )
    )
    ( "UART_BYTES" ::: Df free (BitVector 8)
    , "SPI_DONE" ::: CSignal free Bool
    , ( "SCLK" ::: CSignal free Bool
      , "MOSI" ::: CSignal free Bit
      , "CSB" ::: CSignal free Bool
      )
    )
dut freeClk freeRst skyClk = withBittideByteOrder $ circuit $ \(mm, (jtag, miso)) -> do
  [siBus, timeBus, uartBus, dcBus] <-
    withClockResetEnable freeClk freeRst enableGen
      $ processingElement NoDumpVcd peConfig
      -< (mm, jtag)

  -- The reset in the destination domain (free) of the `domainDiffCounterWbC` is
  -- set by the enables register. The minimum reset duration is trivially
  -- met by the source of `spiDone`.
  let skyRst = xpmResetSynchronizer Asserted freeClk skyClk $ unsafeFromActiveLow spiDone

  (Fwd spiDone, spiOut) <-
    withClockResetEnable freeClk freeRst enableGen
      $ si539xSpiWb (SNat @(Microseconds 10))
      -< (siBus, miso)
  Fwd _localCounter <- withClockResetEnable freeClk freeRst enableGen timeWb -< timeBus
  (uartOut, _uartStatus) <-
    withClockResetEnable freeClk freeRst enableGen
      $ uartInterfaceWb d16 d16 uartBytes
      -< (uartBus, (Fwd (pure Nothing)))
  Fwd _domainDiff <-
    domainDiffCountersWbC (skyClk :> Nil) (skyRst :> Nil) freeClk freeRst -< dcBus

  idC -< (uartOut, Fwd spiDone, spiOut)
 where
  peConfig =
    PeConfig
      { initI = Undefined @IMemWords
      , initD = Undefined @DMemWords
      , iBusTimeout = d0
      , dBusTimeout = d0
      , includeIlaWb = False
      }

type IMemWords = DivRU (64 * 1024) 4
type DMemWords = DivRU (64 * 1024) 4

memoryMap :: MemoryMap
memoryMap = getMMAny $ dut @Basic125 @Basic200 clockGen resetGen clockGen

makeTopEntity 'si539xConfigTest

tests :: HitlTestGroup
tests =
  HitlTestGroup
    { topEntity = 'si539xConfigTest
    , targetXdcs =
        [ "si539xConfigTest.xdc"
        , "jtag" </> "config.xdc"
        , "jtag" </> "pmod1.xdc"
        , "uart" </> "pmod1.xdc"
        , "si539x" </> "spi.xdc"
        ]
    , externalHdl = []
    , testCases =
        [ HitlTestCase
            { name = "Si539xConfigTest"
            , parameters = paramForSingleHwTarget (HwTargetByIndex 7) ()
            , postProcData = ()
            }
        ]
    , mDriverProc = Just D.driverFunc
    , mPostProc = Nothing
    }
