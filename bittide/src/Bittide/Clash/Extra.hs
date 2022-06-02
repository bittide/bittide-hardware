-- SPDX-FileCopyrightText: 2022 Google LLC
-- SPDX-FileCopyrightText: 2020 Christiaan Baaij
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Clash.Extra where

import Clash.Prelude ( Num((-), (+)), Int, Bits(shiftL) )

downto :: (Num a, Bits a) => Int -> Int -> a
downto h l = (1 `shiftL` (h - l + 1) - 1) `shiftL` l
{-# INLINE downto #-}
