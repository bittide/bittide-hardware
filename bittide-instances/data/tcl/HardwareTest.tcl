# SPDX-FileCopyrightText: 2023 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

# The IDs of the Digilent chips on each FPGA board. The indices match the
# position of each FPGA in the mining rig.
set fpga_ids {
  210308B3B272
  210308B0992E
  210308B0AE73
  210308B0AE6D
  210308B0AFD4
  210308B0AE65
  210308B3A22D
  210308B0B0C2
}

set timeout_ms 1000

proc get_part_name {url id} {
  return ${url}/xilinx_tcf/Digilent/${id}
}

# Open the hardware manager and connect to the hardware server at the given url.
# Checks whether the expected number of hardware targets or more are connected,
# if not exit.
proc connect_expected_targets {url expected} {
  open_hw_manager
  connect_hw_server -url $url

  after 1000 refresh_hw_server
  set hw_targets [get_hw_targets -quiet]
  set hw_target_count [llength $hw_targets]
  if {$hw_target_count < $expected} {
    puts "Expected $expected hardware targets, but found $hw_target_count:"
    puts "$hw_targets"
    exit 1
  }
  puts "Hardware server at ${url} hosts ${hw_target_count} hardware targets:"
  puts "$hw_targets"
}

# Set the first board found as the current hardware target and return its device
proc load_first_device {} {
  set target [lindex [get_hw_targets] 0]
  return [load_target_device $target]
}

# Set the target board as the current hardware target and return its device
proc load_target_device {target_name} {
  close_hw_target
  current_hw_target [get_hw_targets $target_name]
  open_hw_target [current_hw_target]
  current_hw_device [lindex [get_hw_devices] 0]
  set device [current_hw_device]
  return $device
}

# Program the current hardware device with the given program and probes file.
proc program_fpga {program_file probes_file} {
  set device [current_hw_device]
  set_property PROGRAM.FILE ${program_file} $device
  set_property PROBES.FILE ${probes_file} $device
  # Program the device and close properly
  program_hw_devices $device
  refresh_hw_device $device
}

# Test one FPGA and returns as list with two flags: test_done and test_success
proc run_test {} {
  # Set the radix of each probe
  set_property INPUT_VALUE_RADIX BINARY [get_hw_probes probe_test_done]
  set_property INPUT_VALUE_RADIX BINARY [get_hw_probes probe_test_success]
  set_property OUTPUT_VALUE_RADIX BINARY [get_hw_probes probe_start_test]

  # Verify that `done` is not set before starting the test
  set_property OUTPUT_VALUE 0 [get_hw_probes probe_start_test]
  commit_hw_vio [get_hw_vios hw_vio_1]
  refresh_hw_vio [get_hw_vios hw_vio_1]

  set done [get_property INPUT_VALUE [get_hw_probes probe_test_done]]
  if {$done != 0} {
    puts "\tERROR: test is done before starting the test"
    exit 1
  }

  # Start the test
  set_property OUTPUT_VALUE 1 [get_hw_probes probe_start_test]
  commit_hw_vio [get_hw_vios hw_vio_1]

  # Refresh the input probes until the done flag is set. Retries for up to 1
  # second.
  set start_time [clock milliseconds]
  set timestamp [format "%s.%03d" \
                    [clock format [expr {$start_time / 1000}] -format %T] \
                    [expr {$start_time % 1000}] \
                ]
  puts "Start time:\t$timestamp"
  while 1 {
    # Check test status, break if test is done
    refresh_hw_vio [get_hw_vios hw_vio_1]
    set done [get_property INPUT_VALUE [get_hw_probes probe_test_done]]
    set success [get_property INPUT_VALUE [get_hw_probes probe_test_success]]
    if {$done == 1} {
      break
    }

    # Timeout if test takes longer than `time_ms`
    global timeout_ms
    set current_time [clock milliseconds]
    set time_spent [expr {$current_time - $start_time}]
    if {${time_spent} > ${timeout_ms}} {
      break
    }
  }
  set current_time [clock milliseconds]
  set timestamp [format "%s.%03d" \
                    [clock format [expr {$current_time / 1000}] -format %T] \
                    [expr {$current_time % 1000}] \
                ]
  puts "End time:\t$timestamp"
  return [list $done $success]
}

# Test all FPGAs one-by-one
proc run_test_all {probes_file fpga_nrs url} {
  global fpga_ids
  set successfull_tests 0
  foreach fpga_nr $fpga_nrs {
    if {$fpga_nr == -1} {
      set device [load_first_device]
    } else {
      set target_id [lindex $fpga_ids $fpga_nr]
      set target_name [get_part_name $url $target_id]
      set device [load_target_device $target_name]
    }

    set_property PROBES.FILE ${probes_file} $device
    refresh_hw_device $device
    set target [current_hw_target]
    puts "Testing FPGA ${fpga_nr} with target ID ${target}"

    set test_results [run_test]
    set done [lindex $test_results 0]
    set success [lindex $test_results 1]
    if {$done == 0} {
      global timeout_ms
      puts "\tTest timeout: done flag not set after ${timeout_ms} ms"
    } elseif {$success == 0} {
      puts "\tTest failed"
    } else {
      puts "\tTest passed"
      incr successfull_tests 1
    }
  }

  set total_tests [llength $fpga_nrs]
  if {$successfull_tests != $total_tests} {
    set failed {$total_tests - $successfull_tests}
    puts "Tests failed for ${failed} targets"
    exit 1
  } else {
    puts "Test passed for all targets"
    exit 0
  }
}
