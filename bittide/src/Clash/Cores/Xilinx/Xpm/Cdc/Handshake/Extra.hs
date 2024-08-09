-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Clash.Cores.Xilinx.Xpm.Cdc.Handshake.Extra where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Xpm
import Data.Maybe

import Bittide.Extra.Maybe

{- | Reliable CDC component based on `xpmCdcHandshakeMaybe` without backpressure and
with limited throughput. Data will be lost if the `src` domain provides more inputs
than the circuit can handle. Useful for low granularity synchronization,
in our case: synchronizing datacounts.
-}
xpmCdcMaybeLossy ::
  ( KnownDomain src
  , KnownDomain dst
  , BitPack a
  , NFDataX a
  , 1 <= BitSize a
  , BitSize a <= 1024
  ) =>
  -- | Source clock
  Clock src ->
  -- | Destination clock
  Clock dst ->
  -- | Data in the source domain
  Signal src (Maybe a) ->
  -- | Data in the destination domain
  Signal dst (Maybe a)
xpmCdcMaybeLossy clkSrc clkDst maybeInp = mux (isRising clkDst noReset enableGen False dstAck) dstOut (pure Nothing)
 where
  srcReg =
    regEn
      clkSrc
      noReset
      enableGen
      Nothing
      (srcRcv .||. srcRegEmpty)
      $ mux (srcRegEmpty .&&. fmap not srcRcv) maybeInp (pure Nothing)

  srcRegEmpty = isNothing <$> srcReg
  (srcRcv, dstOut) = xpmCdcHandshakeMaybe clkSrc clkDst srcReg (isJust <$> dstOut)
  dstAck = isJust <$> dstOut

-- | `Maybe a` based version of `xpmCdcHandshake`.
xpmCdcHandshakeMaybe ::
  ( KnownDomain src
  , KnownDomain dst
  , BitPack a
  , NFDataX a
  , 1 <= BitSize a
  , BitSize a <= 1024
  ) =>
  -- | Source clock
  Clock src ->
  -- | Destination clock
  Clock dst ->
  -- | Data in the source domain
  Signal src (Maybe a) ->
  -- | Acknowledgement from destination domain.
  Signal dst Bool ->
  -- |
  -- 1. Acknowledgement in source domain.
  -- 2. Data in the destination domain.
  (Signal src Bool, Signal dst (Maybe a))
xpmCdcHandshakeMaybe clkSrc clkDst srcIn dstAck = (srcRcv, orNothing <$> dstReq <*> dstOut)
 where
  (dstOut, dstReq, srcRcv) =
    xpmCdcHandshake clkSrc clkDst (fromMaybe (unpack 0) <$> srcIn) (isJust <$> srcIn) dstAck
