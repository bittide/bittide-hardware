-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Gdb (
  -- * GDB process management
  Gdb,
  withGdb,
  withGdbs,

  -- * Running custom commands
  readCommandRaw,
  readCommand,
  readCommand0,
  readCommand1,
  runCommand,
  runCommands,

  -- * Wrappers around common commands
  module Gdb.Commands.Basic,
) where

import Gdb.Internal
import Gdb.Commands.Basic
