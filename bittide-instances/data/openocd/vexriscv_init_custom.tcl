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

set _CHIPNAME vexrisc_ocd

# The JTAG TAP itself is given the name "bridge", because it refers to the
# JtagBridge that's part of the VexRiscv/SpinalHDL debug infrastructure.
# In the example design, there is the JtagBridge controls a single CPU, but
# the capability is there for 1 JTAG TAP + JtagBridge to control multiple
# VexRiscv CPUs.
jtag newtap $_CHIPNAME bridge -expected-id $_CPUTAPID -irlen 5

# There is 1 CPU controlled by the "bridge" JTAG TAP, "cpu0"
target create $_CHIPNAME.cpu0 vexriscv -endian $_ENDIAN -chain-position $_CHIPNAME.bridge

# The JtagBridge/SystemDebugger receives commands in a serialized way. It gets synchronized into
# a parallel bus, and a response is received. Along the way, there may be various clock domain
# crossings or pipeline delays.
# readWaitCycles instructs OpenOCD to insert idle JTAG clock cycles before shifting out
# the response.
# There aren't many transactions where read-back throughput is important, so there's little
# points in lowballing this number.
vexriscv readWaitCycles 10

# When the Verilog of a SpinalHDL design with one or more VexRiscv CPUs is created, the system
# also creates a .yaml file with information that's sideband information that's important for
# OpenOCD to control the CPU correctly.
# A good example of this are the number of hardware breakpoints that are supported by the CPU.
set git_top_level [string trim [exec git rev-parse --show-toplevel]]
vexriscv cpuConfigFile [file join $git_top_level clash-vexriscv clash-vexriscv example-cpu ExampleCpu.yaml]

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
