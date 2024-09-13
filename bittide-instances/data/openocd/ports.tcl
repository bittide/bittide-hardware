# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

set user_gdb_port [env GDB_PORT]
if { $user_gdb_port == "" } {
  error "Required environment variable 'GDB_PORT' is not set."
}

bindto 0.0.0.0
gdb_port $user_gdb_port
tcl_port 6666
telnet_port 4444
