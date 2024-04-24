-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}
{-# LANGUAGE FlexibleInstances #-}
module Bittide.Instances.Hitl.FmcClock where

import Clash.Prelude
import Clash.Explicit.Prelude (orReset, noReset)

import Data.Maybe (fromMaybe, isJust)
import Language.Haskell.TH (runIO)
import System.FilePath

import Protocols
import Protocols.Wishbone

import VexRiscv

import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.UART (ValidBaud)
import Clash.Cores.Xilinx.Ila (IlaConfig(..), Depth(..), ila, ilaConfig)
import Clash.Xilinx.ClockGen (clockWizardDifferential)

import Bittide.ClockControl
import Bittide.ClockControl.Registers (dataCountsWb)
import Bittide.Counter (domainDiffCounter)
import Bittide.DoubleBufferedRam
import Bittide.Hitl (HitlTests, testsFromEnum, hitlVio, singleFpga)
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util (memBlobsFromElf)
import Bittide.SharedTypes (ByteOrder(BigEndian))
import Bittide.Wishbone

import Bittide.Instances.Domains
import Bittide.Instances.Hitl.FincFdec (TestState(..), Test(..), testStateToDoneSuccess)
import Project.FilePath

import Clash.Cores.Xilinx.GTH (ibufds_gte3, gthCore)
import Clash.Explicit.Reset.Extra (Asserted(..), xpmResetSynchronizer)


onChange :: (HiddenClockResetEnable dom, Eq a, NFDataX a) => Signal dom a -> Signal dom Bool
onChange x = (Just <$> x) ./=. register Nothing (Just <$> x)

fmcClockRiscv ::
  forall dom .
  ( HiddenClockResetEnable dom
  , 1 <= DomainPeriod dom
  , ValidBaud dom 921600 ) =>
  "sclBs" ::: BiSignalIn 'PullUp dom 1 ->
  "sdaIn" ::: Signal dom Bit ->
  "datacounts" ::: Vec 1 (Signal dom (DataCount 32)) ->
  "USB_UART_TX" ::: Signal dom Bit ->
  "testSelect" ::: Signal dom (Maybe Test) ->
  ( "sclBs" ::: BiSignalOut 'PullUp dom 1
  , "sdaOut" ::: Signal dom Bit
  , "muxSelect" ::: BitVector 3
  , "clockInitDone" ::: Signal dom Bool
  , "testResult" ::: Signal dom TestState
  , "USB_UART_RX" ::: Signal dom Bit
  )
fmcClockRiscv sclBs sdaIn dataCounts uartRx testSelect =
  ( writeToBiSignal sclBs sclOut
  , fromMaybe 1 <$> sdaOut
  , 0b100
  , clockInitDoneO
  , testResultO
  , uartTx
  )
 where
  (_, (i2cOut, controlReg, uartTx)) =
    circuitFn (((i2cIn, Just <$> testSelect, uartRx), pure (JtagIn low low low)), (pure (), pure (), pure ()))

  Circuit circuitFn = circuit $ \((i2cIn, statusRegIn, uartRx), jtag) -> do
    [timeBus, i2cBus, controlRegBus, statusRegBus, dataCountsBus, uartBus, dummyBus] <- processingElement @dom peConfig -< jtag
    (uartTx, _uartStatus) <- uartWb d16 d16 (SNat @921600) -< (uartBus, uartRx)
    i2cOut <- i2cWb -< (i2cBus, i2cIn)
    dummyWb -< dummyBus
    timeWb -< timeBus
    statusRegWb Nothing -< (statusRegIn, statusRegBus)
    controlReg <- controlRegWb (False, Busy) -< controlRegBus
    dataCountsWb dataCounts -< dataCountsBus
    idC -< (i2cOut, controlReg, uartTx)

  (sclOut, sdaOut) = unbundle i2cOut
  (clockInitDoneO, testResultO) = unbundle controlReg

  sclIn = readFromBiSignal sclBs
  i2cIn :: Signal dom (Bit, Bit)
  i2cIn = bundle (sclIn, fromMaybe <$> sdaIn <*> sdaOut)

  dummyWb :: NFDataX a => Circuit (Wishbone dom 'Standard aw a) ()
  dummyWb = Circuit $ const (pure emptyWishboneS2M, ())

  (iMem, dMem) = $(do
    root <- runIO $ findParentContaining "cabal.project"
    let
      elfDir = root </> firmwareBinariesDir "riscv32imc-unknown-none-elf" Release
      elfPath = elfDir </> "fmc-clock"
      iSize = 128 * 1024 -- 128 KiB
      dSize =  64 * 1024 --  64 KiB
    memBlobsFromElf BigEndian (Just iSize, Just dSize) elfPath Nothing)

  {-
    MSBs   Device
    0b1000 Instruction memory
    0b0100 Data memory
    0b0001 Memory mapped time component
    0b0010 Memory mapped I2C core
    0b0011 Memory mapped control register
    0b0101 Memory mapped status register
    0b0110 Memory mapped datacounts
    0b0111 Memory mapped UART core
    0b0000 Memory mapped dummy device

    Dummy device is needed because the total number of devices cannot be equal to a power
    of 2. The dummy device must then be mapped to the zero-address.
  -}
  peConfig =
    PeConfig
      (0b1000 :> 0b0100 :> 0b0001 :> 0b0010 :> 0b0011 :> 0b0101 :> 0b0110 :> 0b0111 :> (0b0000  :: Unsigned 4) :> Nil)
      (Reloadable $ Blob iMem)
      (Reloadable $ Blob dMem)

fmcClockTests ::
  "SYSCLK_125" ::: DiffClock Ext125 ->
  "FMC_HPC_GBTCLK1_M2C" ::: DiffClock Ext200 ->
  "sclBs" ::: BiSignalIn 'PullUp Basic125 1 ->
  "sdaIn" ::: Signal Basic125 Bit ->
  "USB_UART_TX" ::: Signal Basic125 Bit ->
  ( "sclBs" ::: BiSignalOut 'PullUp Basic125 1
  , "sdaOut" ::: Signal Basic125 Bit
  , "muxSelect" ::: BitVector 3
  , "" :::
    ( "done"    ::: Signal Basic125 Bool
    , "success" ::: Signal Basic125 Bool
    )
  , "USB_UART_RX" ::: Signal Basic125 Bit
  )
fmcClockTests sysClkDiff fmcClkDiff sclBsIn sdaIn uartIn =
  fmcClockIla `hwSeqX`
  (sclBsOut, sdaOut, muxSelect, (testDone, testSuccess), uartOut)
 where
  (sysClk, sysRst :: Reset Basic125) = clockWizardDifferential sysClkDiff noReset

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
    , xpmResetSynchronizer Asserted txClock txClock . unsafeFromActiveLow . fmap unpack -> txClkRst
    ) =
    gthCore
      "X0Y10" "clk0" (pure 0) (pure 0)
      sysClk
      gthRst noReset noReset noReset noReset (pure 0) (pure 0)
      sysClk fmcClk

  gthRst =
    sysRst `orReset`
    unsafeFromActiveLow clockInitDone
  testRst =
    sysRst `orReset`
    unsafeFromActiveLow clockInitDone `orReset`
    unsafeFromActiveLow testStarted

  (domainDiff, dcActive) =
    unbundle $ domainDiffCounter txClock txClkRst sysClk testRst

  testStarted = isJust <$> testInput
  (testDone, testSuccess) = unbundle $ testStateToDoneSuccess <$> testResult

  testInput :: Signal Basic125 (Maybe Test)
  testInput =
    hitlVio
      FDec
      sysClk
      testDone
      testSuccess

  capture :: Signal Basic125 Bool
  capture =
    withClockResetEnable sysClk sysRst enableGen $
    onChange $ bundle
      ( domainDiff
      , testInput
      , testDone
      , testSuccess
      , clockInitDone
      )

  fmcClockIla :: Signal Basic125 ()
  fmcClockIla = setName @"fmcClockIla" $ ila
    (ilaConfig $
         "trigger_0"
      :> "capture_0"
      :> "probe_testInput"
      :> "probe_testDone"
      :> "probe_testSuccess"
      :> "probe_clockInitDone"
      :> "probe_testRst"
      :> "probe_txLockRst"
      :> "probe_domainDiffActive"
      :> "probe_domainDiff"
      :> Nil
    ) { depth = D32768 }
    sysClk
    testStarted
    capture
    -- Debug signals
    testInput
    testDone
    testSuccess
    clockInitDone
    (unsafeToActiveHigh testRst)
    (unsafeToActiveHigh $ xpmResetSynchronizer Asserted txClock sysClk txClkRst)
    dcActive
    domainDiff
{-# NOINLINE fmcClockTests #-}
makeTopEntity 'fmcClockTests

tests :: HitlTests Test
tests = testsFromEnum (singleFpga maxBound)
