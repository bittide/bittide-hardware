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
  target create $_CHIPNAME.tap$i riscv -endian $_ENDIAN -chain-position $_CHIPNAME.tap$i
  $_CHIPNAME.tap$i riscv set_command_timeout_sec 1
}

poll_period 50

init

set tap_list [jtag names]
foreach tap $tap_list {
    set idcode [jtag cget $tap -idcode]
    echo "JTAG_ID: $tap: [format 0x%08X $idcode]"
}

echo "Initialization complete"

# Examine all taps in IDCODE order
# set tap_idcode_pairs {}
# foreach tap $tap_list {
#     set idcode [jtag cget $tap -idcode]
#     lappend tap_idcode_pairs [list $idcode $tap]
# }

# set sorted_tap_idcode_pairs [lsort -integer -index 0 $tap_idcode_pairs]

# foreach pair $sorted_tap_idcode_pairs {
#     set tap [lindex $pair 1]
#     echo "Examining $tap..."
#     $tap arp_examine
# }
