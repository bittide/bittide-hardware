-- SPDX-FileCopyrightText: 2023-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}
module Bittide.Instances.Hitl.FmcClock where

import Clash.Prelude
import Clash.Explicit.Prelude (orReset, noReset)

import Data.Maybe (fromMaybe, isJust)
import Protocols
import Protocols.Internal
import System.Directory
import System.FilePath

import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.UART (ValidBaud)
import Clash.Cores.Xilinx.Ila (IlaConfig(..), Depth(..), ila, ilaConfig)
import Clash.Cores.Xilinx.Xpm (xpmCdcGray)
import Clash.Xilinx.ClockGen (clockWizardDifferential)
import Language.Haskell.TH

import Bittide.ClockControl
import Bittide.Counter (domainDiffCounter)
import Bittide.DoubleBufferedRam
import Bittide.ElasticBuffer (sticky)
import Bittide.Instances.Domains
import Bittide.ClockControl.Registers (dataCountsWb)
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.SharedTypes
import Bittide.Wishbone

import Bittide.Instances.Hitl.FincFdec (TestState(..), Test(..), threshold, testStateToDoneSuccess)

import Clash.Cores.Xilinx.GTH (gthCore)
import Clash.Cores.Xilinx.GTH.Internal (ibufds_gte3)
import Clash.Explicit.Reset.Extra (Asserted(..), xpmResetSynchronizer)
import Clash.Hitl (HitlTests, testsFromEnum, hitlVio, singleFpga)

import qualified Clash.Explicit.Prelude as E


onChange :: (HiddenClockResetEnable dom, Eq a, NFDataX a) => Signal dom a -> Signal dom Bool
onChange x = (Just <$> x) ./=. E.register hasClock hasReset hasEnable Nothing (Just <$> x)

fmcClockRiscv ::
  forall dom n .
  ( HiddenClockResetEnable dom
  , 1 <= DomainPeriod dom
  , ValidBaud dom 921600
  , KnownNat n
  , n <= 32 ) =>
  "sclBs" ::: BiSignalIn 'PullUp dom 1 ->
  "sdaIn" ::: Signal dom Bit ->
  "datacounts" ::: Vec 1 (Signal dom (DataCount n)) ->
  "USB_UART_TX" ::: Signal dom Bit ->
  "testSelect" ::: Signal dom (Maybe Test) ->
  ( "sclBs" ::: BiSignalOut 'PullUp dom 1
  , "sdaOut" ::: Signal dom Bit
  , "muxSelect" ::: BitVector 3
  , "clockInitDone" ::: Signal dom Bool
  , "testResult" ::: Signal dom TestState
  , "USB_UART_RX" ::: Signal dom Bit
  )
fmcClockRiscv sclBs sdaIn dataCounts uartIn testSelect =
  ( writeToBiSignal sclBs sclOut
  , fromMaybe 1 <$> sdaOut
  , 0b100
  , clockInitDoneO
  , testResultO
  , uartOut
  )
 where
  (_, (CSignal (unbundle -> (sclOut, sdaOut)), CSignal (unbundle -> (clockInitDoneO, testResultO)), CSignal uartOut)) = toSignals
    ( circuit $ \(i2cIn, statusRegIn, uartRx) -> do
      [timeBus, i2cBus, controlRegBus, statusRegBus, dataCountsBus, uartBus] <- processingElement @dom peConfig -< ()
      (uartTx, _uartStatus) <- uartWb d16 d16 (SNat @921600) -< (uartBus, uartRx)
      i2cOut <- i2cWb -< (i2cBus, i2cIn)
      timeWb -< timeBus
      statusRegWb CircuitPriority FDec -< (statusRegIn, statusRegBus)
      controlReg <- controlRegWb (False, Busy) -< controlRegBus
      dataCountsWb dataCounts -< dataCountsBus
      idC -< (i2cOut, controlReg, uartTx)
    ) ((CSignal i2cIn, CSignal testSelect, CSignal uartIn), (unitCS, unitCS, unitCS))

  sclIn = readFromBiSignal sclBs
  i2cIn :: Signal dom (Bit, Bit)
  i2cIn = bundle (sclIn, fromMaybe <$> sdaIn <*> sdaOut)

  (   (_iStart, _iSize, iMem)
    , (_dStart, _dSize, dMem)) = $(do

    let
      findProjectRoot :: IO FilePath
      findProjectRoot = goUp =<< getCurrentDirectory
        where
          goUp :: FilePath -> IO FilePath
          goUp path
            | isDrive path = error "Could not find 'cabal.project'"
            | otherwise = do
                exists <- doesFileExist (path </> projectFilename)
                if exists then
                  return path
                else
                  goUp (takeDirectory path)

          projectFilename = "cabal.project"

    root <- runIO findProjectRoot

    let elfPath = root </> "_build/cargo/firmware-binaries/riscv32imc-unknown-none-elf/release/fmc-clock"

    memBlobsFromElf BigEndian elfPath Nothing)

  {-
    MSBs     Device
    0b10_000 Instruction memory
    0b01_000 Data memory
    0b11_000 Memory mapped time component
    0b11_001 Memory mapped I2C core
    0b11_010 Memory mapped control register
    0b11_100 Memory mapped status register
    0b11_101 Memory mapped datacounts
    0b11_101 Memory mapped UART core
  -}
  peConfig =
    PeConfig
      -- (0b100 :> 0b010 :> 0b001 :> 0b011 :> 0b101 :> 0b110 :> 0b111 :> Nil)
      (0b10_000 :> 0b01_000 :> 0b11_000 :> 0b11_001 :> 0b11_010 :> 0b11_011 :> 0b11_100 :> 0b11_101 :> Nil)
      (Reloadable $ Blob iMem)
      (Reloadable $ Blob dMem)

fmcClockTests ::
  "SYSCLK_125" ::: DiffClock Ext125 ->
  "FMC_HPC_GBTCLK1_M2C" ::: DiffClock Ext200 ->
  "sclBs" ::: BiSignalIn 'PullUp Basic124 1 ->
  "sdaIn" ::: Signal Basic124 Bit ->
  "USB_UART_TX" ::: Signal Basic124 Bit ->
  ( "sclBs" ::: BiSignalOut 'PullUp Basic124 1
  , "sdaOut" ::: Signal Basic124 Bit
  , "muxSelect" ::: BitVector 3
  , "" :::
    ( "done"    ::: Signal Basic124 Bool
    , "success" ::: Signal Basic124 Bool
    )
  , "USB_UART_RX" ::: Signal Basic124 Bit
  )
fmcClockTests sysClkDiff fmcClkDiff sclBsIn sdaIn uartIn =
  fmcClockIla `hwSeqX`
  (sclBsOut, sdaOut, muxSelect, (testDone, testSuccess), uartOut)
 where
  (sysClk, sysRst :: Reset Basic124) = clockWizardDifferential sysClkDiff noReset

  (sclBsOut, sdaOut, muxSelect, clockInitDone, testResult, uartOut) =
    withClockResetEnable sysClk sysRst enableGen $
      fmcClockRiscv sclBsIn sdaIn (domainDiff :> Nil) uartIn testInput

  fmcClk = ibufds_gte3 fmcClkDiff :: Clock Ext200

  (   _gthtxn_out
    , _gthtxp_out
    , txClock::Clock GthTx
    , _gtwiz_userclk_rx_usrclk2_out::Clock GthRx
    , _gtwiz_userdata_rx_out
    , _gtwiz_reset_tx_done_out
    , _gtwiz_reset_rx_done_out
    , xpmResetSynchronizer Asserted txClock txClock . unsafeFromActiveLow . fmap unpack -> txLock
    ) =
    gthCore
      "X0Y10" "clk0" (pure 0) (pure 0)
      sysClk
      sysRst noReset noReset noReset noReset (pure 0) (pure 0)
      sysClk fmcClk

  testRst =
    sysRst `orReset`
    unsafeFromActiveLow clockInitDone `orReset`
    unsafeFromActiveLow testStarted
  testRstSync = txLock `orReset` xpmResetSynchronizer Asserted sysClk txClock testRst

  (domainDiff, _dcActive) =
    unbundle $ domainDiffCounter txClock txLock sysClk testRst
  counterSys :: Signal Basic124 (Unsigned 32)
  counterSys = E.register sysClk testRst enableGen 0 (counterSys + 1)
  counterFmc :: Signal GthTx (Unsigned 32)
  counterFmc = E.register txClock testRstSync enableGen 0 (counterFmc + 1)
  counterFmcSync = xpmCdcGray txClock sysClk counterFmc

  testStarted = isJust <$> testInput
  (testDone, testSuccess) = unbundle $ testStateToDoneSuccess <$> testResult

  testInput :: Signal Basic124 (Maybe Test)
  testInput =
    hitlVio
      FDec
      sysClk
      testDone
      testSuccess

  capture ::(Signal Basic124 Bool) =
    withClockResetEnable sysClk sysRst enableGen $
    onChange $ bundle
      ( flip testBit 13 <$> counterSys
      )

  fmcClockIla :: Signal Basic124 ()
  fmcClockIla = setName @"fmcClockIla" $ ila
    (ilaConfig $
         "trigger"
      :> "capture"
      :> "probe_testInput"
      :> "probe_testDone"
      :> "probe_testSuccess"
      :> "probe_counterSys"
      :> "probe_counterFmcSync"
      :> "probe_clockInitDone"
      :> "probe_testRst"
      :> "probe_txLock"
      :> Nil
    ) { depth = D32768 }
    sysClk
    testStarted
    capture
    -- Debug signals
    testInput
    testDone
    testSuccess
    counterSys
    counterFmcSync
    clockInitDone
    (unsafeToActiveHigh testRst)
    (unsafeToActiveHigh $ xpmResetSynchronizer Asserted txClock sysClk txLock)
{-# NOINLINE fmcClockTests #-}
makeTopEntity 'fmcClockTests

tests :: HitlTests Test
tests = testsFromEnum (singleFpga maxBound)
