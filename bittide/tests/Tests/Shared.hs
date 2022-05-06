-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE GADTs #-}
module Tests.Shared where

import Clash.Prelude

data IsInBounds a b c where
  InBounds :: (a <= b, b <= c) => IsInBounds a b c
  NotInBounds :: IsInBounds a b c

deriving instance Show (IsInBounds a b c)

-- | Returns 'InBounds' when a <= b <= c, otherwise returns 'NotInBounds'.
isInBounds :: SNat a -> SNat b -> SNat c -> IsInBounds a b c
isInBounds a b c = case (compareSNat a b, compareSNat b c) of
  (SNatLE, SNatLE) -> InBounds
  _ -> NotInBounds
