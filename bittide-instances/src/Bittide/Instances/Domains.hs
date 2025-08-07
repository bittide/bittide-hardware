-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-orphans #-}

module Bittide.Instances.Domains where

import Clash.Explicit.Prelude hiding (PeriodToCycles)

{- ORMOLU_DISABLE -}
createDomain vXilinxSystem{vName="Basic50",   vPeriod=hzToPeriod  50e6}
createDomain vXilinxSystem{vName="Basic125",  vPeriod=hzToPeriod 125e6}
createDomain vXilinxSystem{vName="Basic125A", vPeriod=hzToPeriod 125e6}
createDomain vXilinxSystem{vName="Basic125B", vPeriod=hzToPeriod 125e6}
createDomain vXilinxSystem{vName="Basic199",  vPeriod=hzToPeriod 199e6}
createDomain vXilinxSystem{vName="Basic200",  vPeriod=hzToPeriod 200e6}
createDomain vXilinxSystem{vName="Basic625",  vPeriod=hzToPeriod 625e6, vResetKind=Asynchronous}

createDomain vXilinxSystem{vName="Ext125",    vPeriod=hzToPeriod 125e6, vResetKind=Asynchronous}
createDomain vXilinxSystem{vName="Ext200",    vPeriod=hzToPeriod 200e6, vResetKind=Asynchronous}
createDomain vXilinxSystem{vName="Ext300",    vPeriod=hzToPeriod 300e6, vResetKind=Asynchronous}

createDomain vXilinxSystem{vName="GthRx",     vPeriod=hzToPeriod 125e6}
createDomain vXilinxSystem{vName="GthTx",     vPeriod=hzToPeriod 125e6}
createDomain vXilinxSystem{vName="GthRx1",    vPeriod=hzToPeriod 250e6}
createDomain vXilinxSystem{vName="GthTx1",    vPeriod=hzToPeriod 250e6}
createDomain vXilinxSystem{vName="GthRxS",    vPeriod=hzToPeriod  10e9}
createDomain vXilinxSystem{vName="GthTxS",    vPeriod=hzToPeriod  10e9}
{- ORMOLU_ENABLE -}

type Bittide = GthTx
