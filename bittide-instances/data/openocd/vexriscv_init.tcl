# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
set  _ENDIAN little
set _TAP_TYPE 1234

if { [info exists CPUTAPID] } {
   set _CPUTAPID $CPUTAPID
} else {
  # set useful default
   set _CPUTAPID 0x10002FFF
}

set _CHIPNAME riscv

# The JTAG TAP itself is given the name "bridge", because it refers to the
# JtagBridge that's part of the VexRiscv/SpinalHDL debug infrastructure.
# In the example design, there is the JtagBridge controls a single CPU, but
# the capability is there for 1 JTAG TAP + JtagBridge to control multiple
# VexRiscv CPUs.
jtag newtap $_CHIPNAME bridge -expected-id $_CPUTAPID -irlen 5

# There is 1 CPU controlled by the "bridge" JTAG TAP, "cpu0"
target create $_CHIPNAME.cpu0 riscv -endian $_ENDIAN -chain-position $_CHIPNAME.bridge

# The rate at which OpenOCD polls active JTAG TAPs to check if there has been a notable
# event. (E.g. to check if the CPU has hit a breakpoint.)
# For some reason, making this number really low has an impact on the CPU while semihosting is
# enabled?
poll_period 50

# Initialize all JTAG TAPs and targets.
init

echo "Halting processor"

# Halts the CPU
halt

# If you also want to reset the CPU, use:
# soft_reset_halt

sleep 1000
