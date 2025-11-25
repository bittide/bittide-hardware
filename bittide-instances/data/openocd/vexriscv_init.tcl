# SPDX-FileCopyrightText: 2025 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
set _ENDIAN little
set _CHIPNAME riscv
set _TAP_TYPE 1234

jtag newtap $_CHIPNAME tap0 -irlen 5 -ignore-version
target create $_CHIPNAME.tap0 riscv -endian $_ENDIAN -chain-position $_CHIPNAME.tap0

poll_period 50

init

set tap_list [jtag names]
foreach tap $tap_list {
    set idcode [jtag cget $tap -idcode]
    echo "JTAG_ID: $tap: [format 0x%08X $idcode]"
}

echo "Initialization complete"
echo "Halting processor"

halt

sleep 1000
