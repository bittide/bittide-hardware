-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-orphans #-}

module Bittide.Instances.Domains where

import Clash.Explicit.Prelude

createDomain vXilinxSystem{vName="Basic125", vPeriod= hzToPeriod 125e6}
createDomain vXilinxSystem{vName="Basic199", vPeriod=hzToPeriod 199e6}
createDomain vXilinxSystem{vName="Basic200", vPeriod=hzToPeriod 200e6}
