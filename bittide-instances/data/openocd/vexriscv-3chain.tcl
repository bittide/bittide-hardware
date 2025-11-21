# SPDX-FileCopyrightText: 2025 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
set user_gdb_port_a [env DEV_A_GDB]
if { $user_gdb_port_a == "" } {
  error "Required environment variable 'DEV_A_GDB' not set."
}

set user_gdb_port_b [env DEV_B_GDB]
if { $user_gdb_port_b == "" } {
  error "Required environment variable 'DEV_B_GDB' not set."
}

set user_gdb_port_c [env DEV_C_GDB]
if { $user_gdb_port_c == "" } {
  error "Required environment variable 'DEV_C_GDB' not set."
}

set user_tcl_port [env TCL_PORT]
if { $user_tcl_port == "" } {
  error "Required environment variable 'TCL_PORT' not set."
}

set user_tel_port [env TEL_PORT]
if { $user_tel_port == "" } {
  error "Required environment variable 'TEL_PORT' not set."
}

set _ENDIAN little
set _TAP_TYPE 1234

bindto 0.0.0.0

if { [info exists CPUTAPID] } {
  set _CPUTAPID $CPUTAPID
} else {
  set _CPUTAPID 0x10002FFF
}

set _CHIPNAME riscv

# Define in physical chain order (these determine scan order)
jtag newtap $_CHIPNAME chain0 -expected-id $_CPUTAPID -irlen 5
jtag newtap $_CHIPNAME chain1 -expected-id $_CPUTAPID -irlen 5
jtag newtap $_CHIPNAME chain2 -expected-id $_CPUTAPID -irlen 5

# Create targets linked to each TAP
target create $_CHIPNAME.cpu0 riscv -endian $_ENDIAN -chain-position $_CHIPNAME.chain0 -gdb-port $user_gdb_port_a
target create $_CHIPNAME.cpu1 riscv -endian $_ENDIAN -chain-position $_CHIPNAME.chain1 -gdb-port $user_gdb_port_b
target create $_CHIPNAME.cpu2 riscv -endian $_ENDIAN -chain-position $_CHIPNAME.chain2 -gdb-port $user_gdb_port_c

tcl port $user_tcl_port
telnet port $user_tel_port

poll_period 50

init

echo "Halting processor"

halt

sleep 1000
