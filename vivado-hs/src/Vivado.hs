-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DuplicateRecordFields #-}

{- | Lets Vivado execute Tcl code by attaching to stdin and stdout of Vivado in
Tcl mode.

There are two main things to keep in mind when working with this module:

  1. Vivado Tcl commands can return objects that, when evaluated, are echoed
     to the Vivado console and log file as a Tcl string due to a feature of
     Tcl called "shimmering". This module can then return them as a Haskell
     string. Commands that expect such objects cannot be passed the shimmered
     string. There are several options to work around this:

     a. Keep the object in Tcl land by storing it in a variable using `set`.

     b. Keep the object in Tcl land by using dedicated Vivado helper commands
        such as `current_hw_server`, `current_hw_target`, `current_hw_ila`,
        etc, to "set" the current object of that type.

     c. Pass the shimmered string from Haskell to a function that can lookup
        the corresponding object again. This can be done using the same
        commands as in the previous option, but now used to get the objects
        instead of setting them.

  2. Bringing objects into Haskell results in shimmering, which changes their
     representation from a faster native Tcl object to a Tcl string. This may
     have performance implications. Furthermore, Vivado truncates shimmered
     strings to the number of characters set in the
     tcl.collectionResultDisplayLimit parameter, which supposedly has a
     default value of 500. This implies that it is challenging to transfer
     large amounts of data from Vivado to Haskell through the approach taken
     by this module.

Refer to the "Vivado Design Suite Tcl Command Reference Guide" (UG835) for
more information.
-}
module Vivado (
  with,
  exec,
  exec_,
  execPrint,
  execPrint_,
  VivadoHandle (..),
  TclException (..),
) where

import Vivado.Internal
