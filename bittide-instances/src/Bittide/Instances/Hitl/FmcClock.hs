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

import Data.Maybe (fromMaybe)
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

import Bittide.Counter (domainDiffCounter)
import Bittide.DoubleBufferedRam
import Bittide.ElasticBuffer (sticky)
import Bittide.Instances.Domains
import Bittide.ProcessingElement
import Bittide.ProcessingElement.Util
import Bittide.SharedTypes
import Bittide.Wishbone

import Clash.Cores.Xilinx.GTH (gthCore)
import Clash.Cores.Xilinx.GTH.Internal (ibufds_gte3)
import Clash.Explicit.Reset.Extra (Asserted(..), xpmResetSynchronizer)
import Clash.Hitl (HitlTests, hitlVioBool, noConfigTest, singleFpga)


onChange :: (HiddenClockResetEnable dom, Eq a, NFDataX a) => Signal dom a -> Signal dom Bool
onChange x = (Just <$> x) ./=. register Nothing (Just <$> x)

fmcClockRiscv ::
  forall dom .
  ( HiddenClockResetEnable dom
  , 1 <= DomainPeriod dom
  , ValidBaud dom 921600 ) =>
  "sclBs" ::: BiSignalIn 'PullUp dom 1 ->
  "sdaIn" ::: Signal dom Bit ->
  "USB_UART_TX" ::: Signal dom Bit ->
  ( "sclBs" ::: BiSignalOut 'PullUp dom 1
  , "sdaOut" ::: Signal dom Bit
  , "muxSelect" ::: BitVector 3
  , "clockInitDone" ::: Signal dom Bool
  , "USB_UART_RX" ::: Signal dom Bit
  )
fmcClockRiscv sclBs sdaIn uartIn =
  ( writeToBiSignal sclBs sclOut
  , fromMaybe 1 <$> sdaOut
  , 0b100
  , clockInitDoneO
  , uartOut
  )
 where
  (_, (CSignal (unbundle -> (sclOut, sdaOut)), CSignal clockInitDoneO, CSignal uartOut)) = toSignals
    ( circuit $ \(i2cIn, uartRx) -> do
      [timeBus, i2cBus, clockInitDoneBus, uartBus] <- processingElement @dom peConfig -< ()
      (uartTx, _uartStatus) <- uartWb d16 d16 (SNat @921600) -< (uartBus, uartRx)
      i2cOut <- i2cWb -< (i2cBus, i2cIn)
      timeWb -< timeBus
      clockInitDone <- controlRegWb False -< clockInitDoneBus
      idC -< (i2cOut, clockInitDone, uartTx)
    ) ((CSignal i2cIn, CSignal uartIn), (unitCS, unitCS, unitCS))

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
    MSBs  Device
    0b100 Instruction memory
    0b010 Data memory
    0b001 Memory mapped time component
    0b011 Memory mapped I2C core
    0b101 Memory mapped clock initialization done reg
    0b110 Memory mapped UART core
  -}
  peConfig =
    PeConfig
      (0b100 :> 0b010 :> 0b001 :> 0b011 :> 0b101 :> 0b110 :> Nil)
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

  (sclBsOut, sdaOut, muxSelect, clockInitDone, uartOut) =
    withClockResetEnable sysClk sysRst enableGen $
      fmcClockRiscv sclBsIn sdaIn uartIn

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
    unsafeFromActiveLow startTest
  testRstSync = txLock `orReset` xpmResetSynchronizer Asserted sysClk txClock testRst

  -- The source domain (125 MHz) runs faster than the destination domain (124 MHz).
  -- Therefore, we expect the counter to become larger. Specifically, we expect
  -- the following: after 124M cycles in the destination domain, the domainDiff
  -- should be around 1M with a margin to be safe.
  expected = 1_000_000
  margin = 10_000

  (domainDiff, dcActive) =
    unbundle $ domainDiffCounter txClock txLock sysClk testRst
  counterSys :: Signal Basic124 (Unsigned 32)
  counterSys = E.register sysClk testRst enableGen 0 (counterSys + 1)

  counterFmc :: Signal GthTx (Unsigned 32)
  counterFmc = E.register txClock testRstSync enableGen 0 (counterFmc + 1)
  counterFmcSync = xpmCdcGray txClock sysClk counterFmc

  inRange = (pure (expected - margin) .<. domainDiff) .&&. (domainDiff .<. pure (expected + margin))

  testDone = sticky sysClk testRst (counterSys .>. 124_000_000)
  testDoneLast = E.register sysClk testRst enableGen False testDone
  testDonePulse = testDone .&&. (not <$> testDoneLast)
  testSuccess = E.register sysClk testRst (toEnable testDonePulse) False inRange

  startTest :: Signal Basic124 Bool
  startTest =
    hitlVioBool
      sysClk
      testDone
      testSuccess

  capture :: Signal Basic125 Bool
  capture =
    withClockResetEnable sysClk sysRst enableGen $
    onChange $ bundle
      ( flip testBit 13 <$> counterSys
      )

  fmcClockIla :: Signal Basic124 ()
  fmcClockIla = setName @"fmcClockIla" $ ila
    (ilaConfig $
         "trigger"
      :> "capture"
      :> "probe_startTest"
      :> "probe_testDone"
      :> "probe_testSuccess"
      :> "probe_counterSys"
      :> "probe_counterFmcSync"
      :> "probe_domainDiff"
      :> "probe_dcActive"
      :> "probe_clockInitDone"
      :> "probe_inRange"
      :> "probe_testRst"
      :> "probe_txLock"
      :> Nil
    ) { depth = D32768 }
    sysClk
    startTest
    capture
    -- Debug signals
    startTest
    testDone
    testSuccess
    counterSys
    counterFmcSync
    domainDiff
    dcActive
    clockInitDone
    inRange
    (unsafeToActiveHigh testRst)
    (unsafeToActiveHigh $ xpmResetSynchronizer Asserted txClock sysClk txLock)

makeTopEntity 'fmcClockTests

tests :: HitlTests ()
tests = noConfigTest "FmcClock" (singleFpga maxBound)
