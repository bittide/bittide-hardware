-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}

{- | A hardware-in-the-loop test to verify we can program the external clock
board from software using a specialized memory-mapped SPI device.
-}
module Bittide.Instances.Hitl.ClockBoardConfiguration where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)

import GHC.Stack (HasCallStack)
import Project.FilePath (
  CargoBuildType (Release),
  findParentContaining,
  firmwareBinariesDir,
 )
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

import Clash.Annotations.TH (makeTopEntity)
import Clash.Class.BitPackC (ByteOrder (BigEndian))
import Clash.Cores.UART (ValidBaud)
import Clash.Cores.Xilinx.Ibufds (ibufdsClock)
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
  hitlVioBool,
  paramForHwTargets,
 )
import Bittide.ProcessingElement (PeConfig (..), processingElement)
import Bittide.ProcessingElement.Util (vecFromElfData, vecFromElfInstr)
import Bittide.SharedTypes (withBittideByteOrder)
import Bittide.Wishbone (timeWb, uartDf, uartInterfaceWb)
#ifdef SIM_BAUD_RATE
import Clash.Cores.UART.Extra
#endif

import Bittide.Instances.Domains
import Bittide.Instances.Hitl.Setup (allHwTargets)

import qualified Bittide.Instances.Hitl.Driver.ClockBoardConfiguration as D

#ifdef SIM_BAUD_RATE
type Baud = MaxBaudRate Basic125
#else
type Baud = 921_600
#endif

baud :: SNat Baud
baud = SNat

clockBoardConfigTest ::
  "CLK_125MHZ" ::: DiffClock Ext125 ->
  "USER_SMA_CLOCK" ::: DiffClock Basic200 ->
  "JTAG" ::: Signal Basic125 JtagIn ->
  "USB_UART_TXD" ::: Signal Basic125 Bit ->
  "MISO" ::: Signal Basic125 Bit ->
  ( "JTAG" ::: Signal Basic125 JtagOut
  , "USB_UART_RXD" ::: Signal Basic125 Bit
  , ""
      ::: ( "SCLK" ::: Signal Basic125 Bool
          , "MOSI" ::: Signal Basic125 Bit
          , "CSB" ::: Signal Basic125 Bool
          )
  )
clockBoardConfigTest freeClkDiff skyClkDiff jtagIn uartRx miso =
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

  ((_mm, (jtagOut, _, _)), (uartTx, _spiDone, spiOut)) =
    toSignals
      (circuitFn freeClk freeRst skyClk)
      (((), (jtagIn, uartRx, miso)), (pure (), pure (), (pure (), pure (), pure ())))

circuitFn ::
  forall free sky.
  ( HasCallStack
  , KnownDomain free
  , KnownDomain sky
  , HasSynchronousReset free
  , HasSynchronousReset sky
  , 1 <= DomainPeriod free
  , ValidBaud free Baud
  ) =>
  Clock free ->
  Reset free ->
  Clock sky ->
  Circuit
    ( "MM" ::: ConstBwd MM
    , ( Jtag free
      , "UART_RX" ::: CSignal free Bit
      , "MISO" ::: CSignal free Bit
      )
    )
    ( "UART_TX" ::: CSignal free Bit
    , "SPI_DONE" ::: CSignal free Bool
    , ( "SCLK" ::: CSignal free Bool
      , "MOSI" ::: CSignal free Bit
      , "CSB" ::: CSignal free Bool
      )
    )
circuitFn freeClk freeRst skyClk = withBittideByteOrder $ circuit $ \(mm, (jtag, uartRx, miso)) -> do
  [ siBus
    , timeBus
    , uartBus
    , dcBus
    ] <-
    withClockResetEnable freeClk freeRst enableGen
      $ processingElement NoDumpVcd peConfig
      -< (mm, jtag)

  let skyRst = xpmResetSynchronizer Asserted freeClk skyClk $ unsafeFromActiveLow spiDone

  (Fwd spiDone, spiOut) <-
    withClockResetEnable freeClk freeRst enableGen
      $ si539xSpiDriverMM (SNat @(Microseconds 10))
      -< (siBus, miso)
  Fwd _localCounter <- withClockResetEnable freeClk freeRst enableGen timeWb -< timeBus
  (uartTx, _uartStatus) <-
    withClockResetEnable freeClk freeRst enableGen
      $ uartInterfaceWb @free d16 d16 (uartDf baud)
      -< (uartBus, uartRx)
  _domainDiff <-
    domainDiffCountersWbC (skyClk :> Nil) (skyRst :> Nil) freeClk freeRst -< dcBus

  idC -< (uartTx, Fwd spiDone, spiOut)
 where
  peConfig
    | clashSimulation = peConfigSim
    | otherwise = peConfigRtl

  peConfigSim = unsafePerformIO $ do
    root <- findParentContaining "cabal.project"
    let
      elfDir = root </> firmwareBinariesDir "riscv32imc" Release
      elfPath = elfDir </> "si5395_test"
    pure
      peConfigRtl
        { initI =
            Reloadable
              $ Vec
              $ unsafePerformIO
              $ vecFromElfInstr @IMemWords BigEndian elfPath
        , initD =
            Reloadable
              $ Vec
              $ unsafePerformIO
              $ vecFromElfData @DMemWords BigEndian elfPath
        , includeIlaWb = False
        }

  peConfigRtl =
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
memoryMap = getMMAny $ circuitFn @Basic125 @Basic200 clockGen resetGen clockGen

makeTopEntity 'clockBoardConfigTest

tests :: HitlTestGroup
tests =
  HitlTestGroup
    { topEntity = 'clockBoardConfigTest
    , targetXdcs =
        [ "clockBoardConfigTest.xdc"
        , "jtag" </> "config.xdc"
        , "jtag" </> "pmod1.xdc"
        , "uart" </> "pmod1.xdc"
        ]
    , externalHdl = []
    , testCases =
        [ HitlTestCase
            { name = "ClockBoardConfigTest"
            , parameters = paramForHwTargets allHwTargets ()
            , postProcData = ()
            }
        ]
    , mDriverProc = Just D.driverFunc
    , mPostProc = Nothing
    }
