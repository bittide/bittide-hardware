-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Bittide.Instances.Hitl.Driver.DnaOverSerial where

import Clash.Prelude

import Bittide.Hitl
import Bittide.Instances.Hitl.Utils.Usb (resetUsbDeviceByLocation)
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.ByteString.Internal (w2c)
import Data.Word8 (isHexDigit)
import Numeric
import System.Exit
import System.IO (BufferMode (..), hSetBuffering)
import System.Timeout
import Test.Tasty.HUnit
import Vivado.Tcl (HwTarget)
import Vivado.VivadoM
import "bittide-extra" Control.Exception.Extra (brackets)

import qualified Bittide.Instances.Hitl.Utils.Serial as Serial
import qualified Data.ByteString as BS
import qualified Data.List as L

{- | Test that all FPGAs that are programmed with `dnaOverSerial` transmit the
DNA that we expect based on the DeviceInfo.
-}
dnaOverSerialDriver ::
  String ->
  [(HwTarget, DeviceInfo)] ->
  VivadoM ExitCode
dnaOverSerialDriver _name targets = do
  -- Reset USB adapter, see documentation of "Bittide.Instances.Hitl.Utils.Usb"
  liftIO $ forM_ targets $ \(_, d) -> resetUsbDeviceByLocation d.usbAdapterLocation

  results <- brackets (liftIO <$> initSerials) (liftIO . snd) $ \initSerialsData -> do
    let targetSerials = fst <$> initSerialsData

    liftIO $ putStrLn "Starting all targets to read DNA values"
    -- start all targets
    forM_ targets $ \(hwT, _) -> do
      openHardwareTarget hwT
      updateVio "vioHitlt" [("probe_test_start", "1")]

    liftIO $ putStrLn "Expecting specific DNAs for all serial ports"
    liftIO $ putStrLn "Serial ports:"
    mapM_ (liftIO . putStrLn) [d.serial | (_, d) <- targets]

    forM (L.zip targets targetSerials) $ \((_, d), serialHandle) -> do
      liftIO $ putStrLn $ "Waiting for output on port: " <> d.serial
      res <- liftIO $ checkDna d serialHandle
      pure res

  liftIO $ print results
  if and results
    then pure ExitSuccess
    else do
      liftIO $ assertFailure "Not all FPGAs transmitted the expected DNA"
      pure $ ExitFailure 2
 where
  -- Must match the gateware's UART baud rate (`dnaOverSerial` uses @SNat \@9600@).
  baud = 9600

  initSerials :: [IO (Serial.SerialHandle, IO ())]
  initSerials = flip L.map targets $ \(_hwT, dI) -> do
    (serialHandle, serialClean) <- Serial.start dI.serial baud

    hSetBuffering serialHandle.handle LineBuffering

    pure (serialHandle, serialClean)

  checkDna :: DeviceInfo -> Serial.SerialHandle -> IO Bool
  checkDna d serialHandle = do
    receivedDna0 <- timeout 10_000_000 $ findDna serialHandle ""
    receivedDna <- case receivedDna0 of
      Just rDna -> return rDna
      Nothing -> assertFailure "Timeout waiting for DNA"
    let
      expected = showHex d.dna ""
      differences = L.zipWith (\e a -> if e == a then ' ' else '^') expected receivedDna
      match = read ("0x" <> receivedDna) == (read ("0x" <> expected) :: Int)
    putStrLn $ "Serial path: " <> d.serial
    putStrLn $ "Expected DNA: " <> expected
    putStrLn $ "Received DNA: " <> receivedDna
    putStrLn $ "Differences:  " <> differences
    pure match

  findDna :: Serial.SerialHandle -> String -> IO String
  findDna serialHandle prev = do
    get <- BS.hGet serialHandle.handle 1
    case BS.unpack get of
      [] -> error "Unexpected end of stream while reading DNA"
      (nC : _) ->
        if isHexDigit nC
          then findDna serialHandle (prev <> [w2c nC])
          else
            if null prev
              then findDna serialHandle prev
              else return prev
