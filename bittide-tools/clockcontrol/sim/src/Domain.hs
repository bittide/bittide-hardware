-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Domain where

import Clash.Explicit.Prelude

createDomain vSystem{
    vName="Bittide"
  , vPeriod=hzToPeriod 200e6
  , vResetKind=Synchronous
  }
