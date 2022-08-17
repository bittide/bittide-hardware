-- SPDX-FileCopyrightText: 2022 Google LLC
-- SPDX-FileCopyrightText: 2020 Christiaan Baaij
--
-- SPDX-License-Identifier: Apache-2.0

module Contranomy.Core.SharedTypes where

import Clash.Prelude

type AddressWidth = 32
type Alignment = BitVector 1
type Bytes = 4
type MachineWord = BitVector 32
type PC = BitVector 32

alignPC :: PC -> PC
alignPC pc = pc' ++# (0 :: Alignment)
 where
  (pc', _) = split pc
