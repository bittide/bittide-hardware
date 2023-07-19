-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Clash.Reset.Extra where

import Clash.Explicit.Prelude

-- TODO: Remove these functions after merging clash-compiler#2539

-- | A reset that is never asserted
noReset :: KnownDomain dom => Reset dom
noReset = unsafeFromActiveHigh (pure False)

-- | Output reset will be asserted when either one of the input resets is
-- asserted
orReset ::
  forall dom .
  ( KnownDomain dom
  , DomainResetKind dom ~ 'Synchronous ) =>
  Reset dom ->
  Reset dom ->
  Reset dom
orReset = unsafeOrReset

-- | Output reset will be asserted when both input resets are asserted
andReset ::
  forall dom .
  ( KnownDomain dom
  , DomainResetKind dom ~ 'Synchronous ) =>
  Reset dom ->
  Reset dom ->
  Reset dom
andReset = unsafeAndReset

-- | Output reset will be asserted when both input resets are asserted. This
-- function is considered unsafe because it can be used on domains with
-- components whose resets are level sensitive, while use of this function can
-- introduce glitches.
unsafeAndReset :: forall dom. KnownDomain dom => Reset dom -> Reset dom -> Reset dom
unsafeAndReset (unsafeFromReset -> rst0) (unsafeFromReset -> rst1) =
  unsafeToReset $
    case resetPolarity @dom of
      SActiveHigh -> rst0 .&&. rst1
      SActiveLow  -> rst0 .||. rst1

-- | Output reset will be asserted when either one of the input resets is
-- asserted. This function is considered unsafe because it can be used on
-- domains with components whose resets are level sensitive, while use of this
-- function can introduce glitches.
unsafeOrReset :: forall dom. KnownDomain dom => Reset dom -> Reset dom -> Reset dom
unsafeOrReset (unsafeFromReset -> rst0) (unsafeFromReset -> rst1) =
  unsafeToReset $
    case resetPolarity @dom of
      SActiveHigh -> rst0 .||. rst1
      SActiveLow  -> rst0 .&&. rst1
