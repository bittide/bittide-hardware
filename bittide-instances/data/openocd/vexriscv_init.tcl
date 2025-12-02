# SPDX-FileCopyrightText: 2025 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
set _ENDIAN little
set _CHIPNAME riscv
set _TAP_TYPE 1234

set user_tap_count [env TAP_COUNT]
if { $user_tap_count == "" } {
  error "Required environment variable 'TAP_COUNT' is not set."
}

for {set i 0} {$i < $user_tap_count} {incr i} {
  jtag newtap $_CHIPNAME tap$i -irlen 5 -ignore-version
  if {$i == [expr {$user_tap_count - 1}]} {
    target create $_CHIPNAME.tap$i riscv -endian $_ENDIAN -chain-position $_CHIPNAME.tap$i
  }
}

poll_period 50

init

set tap_list [jtag names]
foreach tap $tap_list {
    set idcode [jtag cget $tap -idcode]
    echo "JTAG_ID: $tap: [format 0x%08X $idcode]"
}

echo "Initialization complete"
