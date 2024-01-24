-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Instances.Hitl.Setup
  ( FpgaCount
  , fpgaSetup
  ) where

import Clash.Prelude

-- | The number of FPGAs in the current setup
type FpgaCount = 8 :: Nat

-- | Some order of the FPGA ids and a mapping to their connected
-- neighbors (via the index position in the vector) according to the
-- different hardware interfaces on the boards.
fpgaSetup :: Vec FpgaCount (String, Vec (FpgaCount - 1) (Index FpgaCount))
fpgaSetup =
  --   FPGA Id         SFP0    SFP1    J4    J5    J6    J7    SMA
     ( "210308B3B272", 3    :> 2    :> 4  :> 5  :> 6  :> 7  :> 1   :> Nil )
  :> ( "210308B0992E", 2    :> 3    :> 5  :> 6  :> 7  :> 4  :> 0   :> Nil )
  :> ( "210308B0AE73", 1    :> 0    :> 6  :> 7  :> 4  :> 5  :> 3   :> Nil )
  :> ( "210308B0AE6D", 0    :> 1    :> 7  :> 4  :> 5  :> 6  :> 2   :> Nil )
  :> ( "210308B0AFD4", 7    :> 6    :> 0  :> 3  :> 2  :> 1  :> 5   :> Nil )
  :> ( "210308B0AE65", 6    :> 7    :> 1  :> 0  :> 3  :> 2  :> 4   :> Nil )
  :> ( "210308B3A22D", 5    :> 4    :> 2  :> 1  :> 0  :> 3  :> 7   :> Nil )
  :> ( "210308B0B0C2", 4    :> 5    :> 3  :> 2  :> 1  :> 0  :> 6   :> Nil )
  :> Nil
