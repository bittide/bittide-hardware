-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE RecordWildCards #-}

module Bittide.Instances.Pnr.ProcessingElement where

import Clash.Prelude

import Clash.Annotations.TH
import Clash.Class.BitPackC (ByteOrder (BigEndian))
import Clash.Cores.UART (ValidBaud)
import Clash.Explicit.Prelude (noReset, orReset)
import Clash.Xilinx.ClockGen
import Data.Maybe (fromMaybe)
import Protocols
import Protocols.MemoryMap as MM
import System.Environment (lookupEnv)
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import VexRiscv

import Bittide.DoubleBufferedRam
import Bittide.Instances.Domains
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.SharedTypes (withBittideByteOrder)
import Bittide.Wishbone
import Project.FilePath

baudRate :: SNat 921600
baudRate = SNat

vexRiscvUartHelloMM :: MM.MemoryMap
vexRiscvUartHelloMM =
  getMMAny
    $ withClockResetEnable clockGen resetGen enableGen
    $ vexRiscvUartHelloC @Basic200 baudRate

vexRiscvUartHelloC ::
  forall dom baud.
  ( HiddenClockResetEnable dom
  , KnownNat baud
  , ValidBaud dom baud
  ) =>
  SNat baud ->
  Circuit
    (ConstBwd MM, (CSignal dom Bit, Jtag dom))
    (CSignal dom Bit)
vexRiscvUartHelloC baudSnat = withBittideByteOrder $ circuit $ \(mm, (uartRx, jtag)) -> do
  [uartBus, timeBus] <- processingElement NoDumpVcd peConfig -< (mm, jtag)
  (uartTx, _uartStatus) <- uartInterfaceWb d16 d16 (uartDf baudSnat) -< (uartBus, uartRx)
  _localCounter <- timeWb -< timeBus
  idC -< uartTx
 where
  peConfig
    | clashSimulation = peConfigSim
    | otherwise = peConfigRtl

  peConfigSim = unsafePerformIO $ do
    root <- findParentContaining "cabal.project"
    maybeBinaryName <- lookupEnv "TEST_BINARY_NAME"
    let
      elfDir = root </> firmwareBinariesDir "riscv32imc" Debug
      elfPath = elfDir </> fromMaybe "vexrscv-hello" maybeBinaryName
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
      , includeIlaWb = True
      }

{- | A simple instance containing just VexRisc and UART as peripheral.
Runs the `hello` binary from `firmware-binaries`.
-}
vexRiscUartHello ::
  "SYSCLK_125" ::: DiffClock Ext125 ->
  "CPU_RESET" ::: Reset Basic200 ->
  ( ""
      ::: ( "USB_UART_TX" ::: Signal Basic200 Bit
          , "JTAG" ::: Signal Basic200 JtagIn
          )
  , Signal Basic200 ()
  ) ->
  ( ""
      ::: ( "" ::: Signal Basic200 ()
          , "JTAG" ::: Signal Basic200 JtagOut
          )
  , "USB_UART_RX" ::: Signal Basic200 Bit
  )
vexRiscUartHello diffClk rst_in ((uartTx, jtagIn), _) =
  let circuitFn =
        toSignals
          $ withClockResetEnable clk200 rst200 enableGen
          $ vexRiscvUartHelloC baudRate
   in case circuitFn (((), (uartTx, jtagIn)), pure ()) of
        ((_mm, a), b) -> (a, b)
 where
  (clk200, rst200_) = clockWizardDifferential diffClk noReset
  rst200 = rst200_ `orReset` rst_in

type IMemWords = DivRU (64 * 1024) 4
type DMemWords = DivRU (64 * 1024) 4

makeTopEntity 'vexRiscUartHello
