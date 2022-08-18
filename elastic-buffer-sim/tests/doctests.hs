-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main (main) where

import Test.DocTest (mainFromCabal)
import System.Environment (getArgs)

main :: IO ()
main = mainFromCabal "elastic-buffer-sim" =<< getArgs
