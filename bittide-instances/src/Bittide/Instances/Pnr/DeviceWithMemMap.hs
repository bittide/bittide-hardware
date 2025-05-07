-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Bittide.Instances.Pnr.DeviceWithMemMap where

import Bittide.SharedTypes (Bytes)
import Clash.Explicit.Prelude
import GHC.Stack (HasCallStack)
import Protocols
import Protocols.MemoryMap
import Protocols.MemoryMap.Registers.WishboneStandard
import Protocols.Wishbone

initFloat :: Float
initFloat = 3.0

initDouble :: Double
initDouble = 6.0

initU16 :: Unsigned 16
initU16 = 12

initBool :: Bool
initBool = True

initS16 :: Signed 16
initS16 = 500

initEmpty :: Signed 0
initEmpty = 0

{- | A simple device example that uses the Wishbone protocol to read and write
registers. The device has a number of registers with different widths and
types, including floating point, integer, and boolean values.
-}
deviceExample ::
  forall wordSize aw dom.
  ( HasCallStack
  , KnownDomain dom
  , KnownNat wordSize
  , KnownNat aw
  , 1 <= wordSize
  ) =>
  Clock dom ->
  Reset dom ->
  Circuit
    (ConstBwd MM, Wishbone dom 'Standard aw (Bytes wordSize))
    ()
deviceExample clk rst = circuit $ \(mm, wb) -> do
  [float, double, u16, bool, empty, s16] <- deviceWbC "example" -< (mm, wb)

  _f <- registerWbC clk rst (registerConfig "f") initFloat -< (float, Fwd noWrite)
  _d <- registerWbC clk rst (registerConfig "d") initDouble -< (double, Fwd noWrite)
  _u <- registerWbC clk rst (registerConfig "u") initU16 -< (u16, Fwd noWrite)
  _b <- registerWbC clk rst (registerConfig "b") initBool -< (bool, Fwd noWrite)
  -- XXX: Empty register causes warnings in the Clash compiler, see:
  --      https://github.com/clash-lang/clash-compiler/issues/2956
  _e <- registerWbC clk rst (registerConfig "e") initEmpty -< (empty, Fwd noWrite)
  _s <- registerWbC clk rst (registerConfig "s") initS16 -< (s16, Fwd noWrite)

  idC
 where
  noWrite = pure Nothing

deviceWithMemMap ::
  Clock XilinxSystem ->
  Reset XilinxSystem ->
  Signal XilinxSystem (WishboneM2S 3 4 (BitVector 32)) ->
  Signal XilinxSystem (WishboneS2M (BitVector 32))
deviceWithMemMap clk rst m2s =
  snd $ fst $ toSignals (deviceExample @4 @3 @XilinxSystem clk rst) (((), m2s), ())
