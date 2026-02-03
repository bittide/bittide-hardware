-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Clash.Explicit.Signal.Delayed.Extra where

import Clash.Explicit.Prelude
import qualified Clash.Explicit.Signal.Delayed as D

-- | Creates a delay annotated circuit from a block RAM primitive.
fromBlockram ::
  (KnownDomain dom, Num addr, NFDataX addr, NFDataX a, KnownNat d) =>
  (Signal dom addr -> Signal dom (Maybe (addr, a)) -> Signal dom a) ->
  ( D.DSignal dom d addr ->
    D.DSignal dom d (Maybe (addr, a)) ->
    D.DSignal dom (d + 1) a
  )
fromBlockram prim readAddr writeAddr = D.forward SNat (D.fromSignal dataOut)
 where
  dataOut = prim (D.toSignal readAddr) (D.toSignal writeAddr)

{- | Creates a delay annotated circuit from a block RAM primitive. Supports byte enables
for its write input.
-}
fromBlockramWithMask ::
  forall dom addr n d.
  (KnownDomain dom, Num addr, NFDataX addr, KnownNat n, KnownNat d) =>
  ( Signal dom addr ->
    Signal dom (Maybe (addr, BitVector (n * 8))) ->
    Signal dom (BitVector n) ->
    Signal dom (BitVector (n * 8))
  ) ->
  ( DSignal dom d addr ->
    DSignal dom d (Maybe (addr, (BitVector (n * 8)))) ->
    DSignal dom d (BitVector n) ->
    DSignal dom (d + 1) (BitVector (n * 8))
  )
fromBlockramWithMask prim readAddr writeAddr writeMask = D.forward SNat (D.fromSignal dataOut)
 where
  dataOut = prim (D.toSignal readAddr) (D.toSignal writeAddr) (D.toSignal writeMask)
