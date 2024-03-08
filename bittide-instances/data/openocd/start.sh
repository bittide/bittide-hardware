#!/usr/bin/env bash
cd $(dirname $0)
openocd-vexriscv -f ports.tcl -f sipeed.tcl -f vexriscv_init.tcl $@
