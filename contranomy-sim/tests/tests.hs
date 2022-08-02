-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

import Prelude
import Test.Tasty (defaultMain, testGroup)
import Tests.ContranomySim.Print qualified
import Tests.ContranomySim.ReadElf qualified

main :: IO ()
main = do
  let tests  = testGroup "ContranomySim Tests"
                [ Tests.ContranomySim.Print.tests
                , Tests.ContranomySim.ReadElf.tests
                ]
  defaultMain tests
