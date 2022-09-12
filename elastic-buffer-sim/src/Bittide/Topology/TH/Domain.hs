-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bittide.Topology.TH.Domain where

import Clash.Explicit.Prelude

-- 200kHz instead of 200MHz; otherwise the periods are so small that deviations
-- can't be expressed as 'Natural's
createDomain vSystem{vName="Bittide", vPeriod=hzToPeriod 200e3, vResetKind=Synchronous}
