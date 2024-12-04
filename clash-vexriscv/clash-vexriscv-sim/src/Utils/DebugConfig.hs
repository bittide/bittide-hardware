-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE GADTs #-}

module Utils.DebugConfig where

import Prelude

--------------------------------------
--
-- Debugging configuration
--
--------------------------------------

data DebugConfiguration where
  -- | Run a program and only output what the program writes to the
  --   character-device
  RunCharacterDevice :: DebugConfiguration
  -- | Run a program and print detailed information of CPU bus interactions
  InspectBusses ::
    -- | # of cycles the program takes to initialise the instruction memory
    Int ->
    -- | # of "uninteresting" cycles to skip, such as runtime setup code
    Int ->
    -- | # of "interesting" cycles to inspect
    Maybe Int ->
    -- | inspect instruct-bus interactions
    Bool ->
    -- | inspect data-bus interactions
    Bool ->
    DebugConfiguration
  -- | Run a program and output all the write operations
  InspectWrites :: DebugConfiguration
