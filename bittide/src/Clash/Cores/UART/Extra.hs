-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}
module Clash.Cores.UART.Extra
  ( module Clash.Cores.UART.Extra
  , System.IO.stdin
  , System.IO.stdout
  ) where

import Clash.Prelude

import Clash.Cores.UART
import Data.Char
import Data.Maybe
import System.IO
import Protocols
import Protocols.Df hiding (catMaybes, sample, pure)
import Protocols.Internal
import GHC.IO

import Bittide.Wishbone

import qualified Protocols.Df as Df

-- | A simulation function for circuits that expose a UART connection.
-- This function reads from the provided input handle and feeds that to the UART circuit.
-- Incoming uart data is written the the output handle.
uartIO ::
  forall dom baud .
  (KnownDomain dom, ValidBaud dom baud) =>
  -- | A handle for the input data stream, use `stdin` for terminal input.
  Handle ->
  -- | A handle for the output data stream, use `stdout` for terminal output.
  Handle ->
  -- | The baud rate for the UART communication.
  SNat baud ->
  -- | The circuit to be simulated.
  Circuit (CSignal dom Bit) (CSignal dom Bit) ->
  -- | The IO action that performs simulation.
  IO ()
uartIO inputHandle outputHandle baud uartCircuit = do
  printList . catMaybes $ Df.sample def $ ioCircuit clockGen resetGen
 where
  printList :: [(BitVector 8)] -> IO ()
  printList [] = pure ()
  printList (x:xs) = do
    hPutChar outputHandle . chr $ fromIntegral x
    hFlush outputHandle
    printList xs

  ioList :: Handle -> [IO (Maybe Char)]
  ioList h = flip (:) (ioList h) $ do
    charReady <- hReady h
    if charReady
      then (fmap Just) $ hGetChar h
      else pure Nothing

  input = fmap unsafePerformIO $ ioList inputHandle
  ioCircuit = exposeClock $ exposeReset $ circuit $ \_n -> do
    dfIn <- drive def (fmap (fmap (fromIntegral . ord)) input)
    txBit <- uartCircuit -< rxBit
    (receivedByte, rxBit) <- exposeEnable uartDf enableGen baud -< (dfIn, txBit)
    unsafeToDf -< receivedByte
