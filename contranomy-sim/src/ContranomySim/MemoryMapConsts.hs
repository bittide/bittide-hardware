-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{- | Address-constants for memory mapped components.

     Ideally a device tree configuration would be the "source of truth"
     for both the consuming firmware and the providing simulation.
     However, there currently is no published package on Hackage for reading
     FDTs and maintaining an FDT parser is outside of the scope of this project.

     As a "workaround" all the address constants are kept here in a central
     module.
-}

{-# LANGUAGE NumericUnderscores #-}
module ContranomySim.MemoryMapConsts where

import           Clash.Prelude (Unsigned)
import           Data.IntMap   (Key)

fdtAddr :: Key
fdtAddr = 0x00008000

characterDeviceAddr :: Unsigned 32
characterDeviceAddr = 0x7000_0000
