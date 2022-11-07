-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-orphans #-}

module Bittide.Instances.Domains where

import Clash.Explicit.Prelude

createDomain vXilinxSystem{vName="Basic200", vPeriod=hzToPeriod 200e6}
createDomain vXilinxSystem{vName="Basic100", vPeriod=hzToPeriod 100e6}
