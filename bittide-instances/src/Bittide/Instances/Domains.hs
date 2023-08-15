-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-orphans #-}

module Bittide.Instances.Domains where

import Clash.Explicit.Prelude

createDomain vXilinxSystem{vName="Basic125", vPeriod= hzToPeriod 125e6}
createDomain vXilinxSystem{vName="Basic25", vPeriod= hzToPeriod 25e6}
createDomain vXilinxSystem{vName="Basic199", vPeriod=hzToPeriod 199e6}
createDomain vXilinxSystem{vName="Basic200", vPeriod=hzToPeriod 200e6}
createDomain vXilinxSystem{vName="Basic200A", vPeriod=hzToPeriod 200e6}
createDomain vXilinxSystem{vName="Basic200B", vPeriod=hzToPeriod 200e6}
createDomain vXilinxSystem{vName="Basic300", vPeriod=hzToPeriod 300e6}
createDomain vXilinxSystem{vName="Basic50", vPeriod= hzToPeriod 50e6}
createDomain vXilinxSystem{vName="External", vPeriod=hzToPeriod 200e6}
createDomain vXilinxSystem{vName="GthRx", vPeriod=hzToPeriod 125e6}
createDomain vXilinxSystem{vName="GthTx", vPeriod= hzToPeriod 125e6}
createDomain vXilinxSystem{vName="Internal", vPeriod=hzToPeriod 200e6}
