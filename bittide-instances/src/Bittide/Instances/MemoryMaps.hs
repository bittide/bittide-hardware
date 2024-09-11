-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Bittide.Instances.MemoryMaps where

import Clash.Prelude

import Bittide.Instances.MemoryMapLogic (processMemoryMaps)

$( do
    processMemoryMaps
    pure []
 )
