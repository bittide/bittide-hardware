-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Clash.Cores.Xilinx.Xpm.Cdc.Extra (
  safeXpmCdcHandshake,
  xpmCdcHandshakeDf,
) where

import Clash.Explicit.Prelude

import Clash.Class.Cdc.Handshake (safeHandshake)
import Clash.Cores.Xilinx (withXilinx)
import GHC.Stack (HasCallStack)
import Protocols

{- | A wrapper around 'xpmCdcHandshake' that implements a 'Df'-like interface:

  * If a 'Just' is given as an argument it should remain stable until an Ack is
    returned. This means that it can neither transition to 'Nothing', nor can the
    contents inside of the 'Just' change while @'Ack' 'False'@ is returned.

  * The input value (@'Maybe' a@) cannot depend combinationally on the returned
    'Ack'.

This implements the state machine necessary to safely do a handshake, as described in
the Xilinx documentation.

If either side is reset, the other should be too. Though it doesn't matter in which
order the resets are deasserted, the resets should be asserted for long enough for the
handshake pipeline to flush. See the Xilinx documentation for more information.
-}
safeXpmCdcHandshake ::
  forall a src dst.
  ( 1 <= BitSize a
  , BitSize a <= 1024
  , KnownDomain src
  , KnownDomain dst
  , BitPack a
  , NFDataX a
  , HasCallStack
  ) =>
  Clock src ->
  Reset src ->
  Clock dst ->
  Reset dst ->
  Signal src (Maybe a) ->
  Signal dst Ack ->
  ( Signal src Ack
  , Signal dst (Maybe a)
  )
safeXpmCdcHandshake = withXilinx safeHandshake

{- | 'Df' version of 'xpmCdcHandshake'.

If either side is reset, the other should be too. Though it doesn't matter in which
order the resets are deasserted, the resets should be asserted for long enough for the
handshake pipeline to flush. See the Xilinx documentation for more information.
-}
xpmCdcHandshakeDf ::
  forall a src dst.
  ( 1 <= BitSize a
  , BitSize a <= 1024
  , KnownDomain src
  , KnownDomain dst
  , BitPack a
  , NFDataX a
  , HasCallStack
  ) =>
  Clock src ->
  Reset src ->
  Clock dst ->
  Reset dst ->
  Circuit (Df src a) (Df dst a)
xpmCdcHandshakeDf clkSrc rstSrc clkDst rstDst = Circuit go
 where
  go :: (Signal src (Maybe a), Signal dst Ack) -> (Signal src Ack, Signal dst (Maybe a))
  go (dat, ack) = safeXpmCdcHandshake clkSrc rstSrc clkDst rstDst dat ack
