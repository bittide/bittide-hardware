-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

import Prelude
import System.Environment (getArgs)
import Clash.Main (defaultMain)

main :: IO ()
main = getArgs >>= defaultMain
