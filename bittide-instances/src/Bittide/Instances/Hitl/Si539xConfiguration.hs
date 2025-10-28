-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}

{- | A hardware-in-the-loop test to verify we can program the external clock
board from software using a specialized memory-mapped SPI device.
-}
module Bittide.Instances.Hitl.Si539xConfiguration where

import Clash.Explicit.Prelude
import Clash.Prelude (
  HiddenClockResetEnable,
  hasClock,
  hasEnable,
  hasReset,
  withClockResetEnable,
 )

import GHC.Stack (HasCallStack)
import System.FilePath ((</>))

import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.Xilinx.Ibufds (ibufdsClock)
import Clash.Cores.Xilinx.Ila (ila, ilaConfig)
import Clash.Cores.Xilinx.Xpm.Cdc.ArraySingle (xpmCdcArraySingle)
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

#ifdef SIM_BAUD_RATE
type Baud = MaxBaudRate Basic125
#else
type Baud = 921_600
#endif

baud :: SNat Baud
baud = SNat

si539xConfigTest ::
  "CLK_125MHZ" ::: DiffClock Ext125 ->
  "USER_SMA_CLOCK" ::: DiffClock Basic200 ->
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
  skyClk = ibufdsClock skyClkDiff

  -- We do all test control in software and don't even have useful control
  -- signals in hardware. So we just set some default values and ignore them
  -- in the driver.
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
      ((jtagIn, miso), (pure (), (pure (), pure (), pure ())))

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
  [ siBus
    , timeBus
    , uartBus
    , dcBus
    ] <-
    withClockResetEnable freeClk freeRst enableGen
      $ processingElement NoDumpVcd peConfig
      -< (mm, jtag)

  let skyRst = xpmResetSynchronizer Asserted freeClk skyClk $ unsafeFromActiveLow spiDone
  -- XXX: This is really ugly, but I can't think of another way to make sure the
  -- domain diff counter is reset properly.
  let skyRstFree = xpmResetSynchronizer Asserted skyClk freeClk $ skyRst

  let
    onChange ::
      (HiddenClockResetEnable dom, Eq a, NFDataX a) => Signal dom a -> Signal dom Bool
    onChange x = (Just <$> x) ./=. register hasClock hasReset hasEnable Nothing (Just <$> x)

    capture =
      withClockResetEnable freeClk freeRst enableGen
        $ onChange
        $ bundle (spiDone, (unsafeToActiveLow skyRstFree), diffActive, counterSkyFree)

    diffCounter = (fmap (fst . head)) domainDiff
    diffActive = (fmap (snd . head)) domainDiff

    counterSky :: Signal sky (Unsigned 32)
    counterSky = register skyClk skyRst enableGen 0 $ counterSky + 1
    counterSkyFree = xpmCdcArraySingle skyClk freeClk counterSky

    spiTestIla :: Signal free ()
    spiTestIla =
      setName @"spiTestIla"
        $ ila
          ( ilaConfig
              $ "trigger"
              :> "capture"
              :> "spiDone"
              :> "spiDoneSky"
              :> "localCounterFree"
              :> "localCounterSky"
              :> "diffCounter"
              :> "diffActive"
              :> Nil
          )
          freeClk
          capture
          capture
          spiDone
          (unsafeToActiveLow skyRstFree)
          localCounter
          counterSkyFree
          diffCounter
          diffActive

  (Fwd spiDone, spiOut) <-
    withClockResetEnable freeClk freeRst enableGen
      $ si539xSpiWb (SNat @(Microseconds 10))
      -< (siBus, miso)
  Fwd localCounter <- withClockResetEnable freeClk freeRst enableGen timeWb -< timeBus
  (uartOut, _uartStatus) <-
    withClockResetEnable freeClk freeRst enableGen
      $ uartInterfaceWb d16 d16 uartBytes
      -< (uartBus, (Fwd (pure Nothing)))
  Fwd domainDiff <-
    domainDiffCountersWbC (skyClk :> Nil) (skyRst :> Nil) freeClk skyRstFree -< dcBus

  idC -< (uartOut, Fwd (hwSeqX spiTestIla spiDone), spiOut)
 where
  peConfig =
    PeConfig
      { initI = Undefined @IMemWords
      , initD = Undefined @DMemWords
      , iBusTimeout = d0
      , dBusTimeout = d0
      , includeIlaWb = False
      }

type IMemWords = DivRU (128 * 1024) 4
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
