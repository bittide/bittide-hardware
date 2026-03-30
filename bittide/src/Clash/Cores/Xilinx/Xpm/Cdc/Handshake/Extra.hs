-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Clash.Cores.Xilinx.Xpm.Cdc.Handshake.Extra where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx (Xilinx, withXilinx)

import qualified Clash.Class.Cdc as Cdc

-- | Xilinx-specialized version of 'maybeLossy'
xpmCdcMaybeLossy ::
  ( KnownDomain src
  , KnownDomain dst
  , BitPack a
  , NFDataX a
  , Cdc.ValidHandshake Xilinx a src dst
  ) =>
  -- | Source clock
  Clock src ->
  -- | Destination clock
  Clock dst ->
  -- | Data in the source domain
  Signal src (Maybe a) ->
  -- | Data in the destination domain
  Signal dst (Maybe a)
xpmCdcMaybeLossy = withXilinx Cdc.maybeLossy

-- | Xilinx-specialized version of 'handshake
xpmCdcHandshakeMaybe ::
  ( KnownDomain src
  , KnownDomain dst
  , BitPack a
  , NFDataX a
  , Cdc.ValidHandshake Xilinx a src dst
  ) =>
  -- | Source clock
  Clock src ->
  -- | Destination clock
  Clock dst ->
  -- | Data in the source domain
  Signal src (Maybe a) ->
  -- | Acknowledgement from destination domain.
  Signal dst Bool ->
  {- |
  1. Acknowledgement in source domain.
  2. Data in the destination domain.
  -}
  (Signal src Bool, Signal dst (Maybe a))
xpmCdcHandshakeMaybe = withXilinx Cdc.handshakeMaybe
