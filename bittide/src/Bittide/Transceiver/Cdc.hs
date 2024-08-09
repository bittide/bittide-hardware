-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | Utilities to safely synchronize signals between different clock domains,
where either the source or the destination clock is inactive.
-}
module Bittide.Transceiver.Cdc (withLock) where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Xpm.Cdc.Single (XpmCdcSingleConfig (..), xpmCdcSingleWith)

{- | Synchronize an (asynchronous) lock signal between two clock domains. This is
different from using 'xpmCdcSingle' directly, as it ensures that the registers
in the destination domain are properly reset.
-}
withLock ::
  ( KnownDomain src
  , KnownDomain dst
  ) =>
  Clock src ->
  Signal src Bool ->
  Clock dst ->
  Reset dst ->
  Signal src Bool ->
  Signal dst Bool
withLock = withLockN (SNat @4)

-- | Worker function for 'cdcLock'.
withLockN ::
  forall src dst nRegs.
  ( KnownDomain src
  , KnownDomain dst
  , 2 <= nRegs
  , nRegs <= 10
  ) =>
  SNat nRegs ->
  Clock src ->
  Signal src Bool ->
  Clock dst ->
  Reset dst ->
  Signal src Bool ->
  Signal dst Bool
withLockN nRegs@SNat srcClk asyncSrcRst dstClk dstRst s =
  -- Ignore the first /nRegs/ cycles of 'lock' - the pipeline might still contain
  -- old data. Ideally 'xpmCdcSingle' would have a reset port so we could avoid
  -- this.
  mux
    (counter .==. pure (maxBound @(Index (nRegs + 1))))
    lockSynced
    (pure False)
 where
  counter = register dstClk dstRst enableGen 0 (satSucc SatBound <$> counter)

  cdcConfig =
    XpmCdcSingleConfig
      { stages = nRegs
      , initialValues = True -- default
      , registerInput = False
      }

  sGlitchFree = delay srcClk enableGen False s
  lockSynced = xpmCdcSingleWith cdcConfig srcClk dstClk (asyncSrcRst .&&. sGlitchFree)
