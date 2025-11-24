# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

set user_gdb_port [env GDB_PORT]
if { $user_gdb_port == "" } {
  error "Required environment variable 'GDB_PORT' is not set."
}

set user_tcl_port [env TCL_PORT]
if { $user_tcl_port == "" } {
  error "Required environment variable 'TCL_PORT' is not set."
}

set user_telnet_port [env TELNET_PORT]
if { $user_telnet_port == "" } {
  error "Required environment variable 'TELNET_PORT' is not set."
}

bindto 0.0.0.0
gdb port $user_gdb_port
tcl port $user_tcl_port
telnet port $user_telnet_port
