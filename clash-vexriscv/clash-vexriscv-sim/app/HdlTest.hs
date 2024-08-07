-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

import Prelude

import qualified Clash.Main as Clash

main :: IO ()
main = Clash.defaultMain ["Utils.Instance", "-main-is", "circuit", "--verilog"]
