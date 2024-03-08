-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RecordWildCards #-}
module VexRiscv.JtagTcpBridge (vexrJtagBridge, defaultIn) where

import Clash.Prelude

import Clash.Signal.Internal

import VexRiscv
import VexRiscv.FFI

import Foreign
import System.IO.Unsafe (unsafePerformIO)
import Network.Socket (PortNumber)

defaultIn :: JtagIn
defaultIn = JtagIn { testClock = low, testModeSelect = low, testDataIn = low }

{-# NOINLINE inner #-}
inner :: (t -> IO JtagIn) -> Signal dom t -> Signal dom JtagIn
inner jtagBridgeStep (o :- outs) = unsafePerformIO $ do
    in' <- jtagBridgeStep o
    let ins' = inner jtagBridgeStep outs
    pure $ in' :- (in' `deepseqX` ins')

vexrJtagBridge :: PortNumber -> Signal dom JtagOut -> Signal dom JtagIn
vexrJtagBridge port out = inner jtagBridgeStep out
 where
  (_, jtagBridgeStep) = unsafePerformIO $ vexrJtagBridge' port

vexrJtagBridge' ::
    PortNumber ->
    IO ( IO () -- ^ delete function
       , JtagOut -> IO JtagIn -- ^ step function
       )
vexrJtagBridge' port = do
    bridge <- vexrJtagBridgeInit (fromIntegral port)
    let
        shutDown = vexrJtagBridgeShutdown bridge

        step JtagOut{..} = alloca $ \outFFI -> alloca $ \inFFI -> do
            poke outFFI (JTAG_OUTPUT debugReset testDataOut)
            vexrJtagBridgeStep bridge outFFI inFFI
            JTAG_INPUT{..} <- peek inFFI
            let input = JtagIn { testClock = tck, testModeSelect = tms, testDataIn = tdi }
            pure input
    pure (shutDown, step)
