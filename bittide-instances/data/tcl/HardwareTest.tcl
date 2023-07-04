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

# Prints all VIOs in the radix they are set. A current hardware device must be
# set before calling this function.
proc print_all_vios {} {
    set probes [get_hw_probes]

    # Find the maximum widths of each column, with a minimum of the header length
    set w_name 4
    set w_value 5
    set w_radix 5
    foreach probe $probes {
        set type [get_property type $probe]
        set w_name_cur [string length [get_property name $probe]]
        if {$type == "vio_input"} {
            set w_value_cur [string length [get_property input_value $probe]]
            set w_radix_cur [string length [get_property input_value_radix $probe]]
        } else {
            set w_value_cur [string length [get_property output_value $probe]]
            set w_radix_cur [string length [get_property output_value_radix $probe]]
        }
        set w_name [expr max($w_name, $w_name_cur)]
        set w_value [expr max($w_value, $w_value_cur)]
        set w_radix [expr max($w_radix, $w_radix_cur)]
    }

    puts "Printing all probes"
    set sep +-[string repeat - $w_name]-+-[string repeat - $w_value]-+-[string repeat - $w_radix]-+
    puts $sep
    puts [format "| %-*s | %-*s | %-*s |" $w_name "Name" $w_value "Value" $w_radix "Radix"]
    puts $sep

    set input_probes [get_hw_probes -filter {type == vio_input}]
    foreach input_probe $input_probes {
        set name [get_property name $input_probe]
        set value [get_property input_value $input_probe]
        set radix [get_property input_value_radix $input_probe]
        puts [format "| %-*s | %*s | %-*s |" $w_name $name $w_value $value $w_radix $radix]
    }
    puts $sep

    set output_probes [get_hw_probes -filter {type == vio_output}]
    foreach output_probe $output_probes {
        set name [get_property name $output_probe]
        set value [get_property output_value $output_probe]
        set radix [get_property output_value_radix $output_probe]
        puts [format "| %-*s | %*s | %-*s |" $w_name $name $w_value $value $w_radix $radix]
    }
    puts $sep
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

# Format a time given in millseconds to a human-readable string
proc format_time {time_ms} {
    return [format "%s.%03d" \
                     [clock format [expr {$time_ms / 1000}] -format %T] \
                     [expr {$time_ms % 1000}] \
                 ]
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

proc run_single_test {start_probe} {
    # Verify that `done` is not set before starting the test
    set_property OUTPUT_VALUE 0 $start_probe
    commit_hw_vio [get_hw_vios hw_vio_1]
    refresh_hw_vio [get_hw_vios hw_vio_1]
    set done [get_property INPUT_VALUE [get_hw_probes probe_test_done]]
    if {$done != 0} {
        puts "\tERROR: test is done before starting the test"
        print_all_vios
        exit 1
    }

    # Start the test
    set_property OUTPUT_VALUE 1 $start_probe
    commit_hw_vio [get_hw_vios hw_vio_1]

    # Refresh the input probes until the done flag is set. Retries for up to
    # `time_ms` milliseconds.
    set start_time [clock milliseconds]
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

    # Print test results. Prints all VIO probes when a test fails
    if {$done == 0} {
        set current_time [clock milliseconds]
        global timeout_ms
        puts "\tTest timeout: done flag not set after ${timeout_ms} ms"
        set timestamp_start [format_time $start_time]
        puts "\tStarted test: $timestamp_start"
        set timestamp_end [format_time $current_time]
        puts "\tEnded test:     $timestamp_end"
        print_all_vios
    } elseif {$success == 0} {
        puts "\tTest failed"
        print_all_vios
    } else {
        puts "\tTest passed"
    }

    # Reset the start probe for the current test
    set_property OUTPUT_VALUE 0 $start_probe
    commit_hw_vio [get_hw_vios hw_vio_1]

    return [list $done $success]
}

# Test one FPGA, where each probe which name starts with 'probe_test_start'
# indicates one hardware test. Hardware tests are run sequentially.
proc run_device_tests {} {
    # Set the radix of each probe
    set_property INPUT_VALUE_RADIX BINARY [get_hw_probes probe_test_done]
    set_property INPUT_VALUE_RADIX BINARY [get_hw_probes probe_test_success]

    set start_probes [get_hw_probes probe_test_start*]
    set successfull_tests 0
    foreach start_probe $start_probes {
        set probe_name [get_property name $start_probe]
        puts "Running test: ${probe_name}"
        set_property OUTPUT_VALUE_RADIX BINARY $start_probe
        set res [run_single_test $start_probe]
        set done [lindex $res 0]
        set success [lindex $res 1]
        if {[expr $done == 1] && [expr $success == 1]} {
            incr successfull_tests 1
        }
    }
    set all_success [expr [llength $start_probes] == $successfull_tests]
    return $all_success
}

# Test all FPGAs one-by-one
proc run_test_all {probes_file fpga_nrs url} {
    global fpga_ids
    set successful_devices 0
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

        set device_test_success [run_device_tests]
        if {$device_test_success == 1} {
            incr successful_devices
        }
    }

    set devices [llength $fpga_nrs]
    if {$successful_devices != $devices} {
        set failed [expr {$devices - $successful_devices}]
        puts "Tests failed for ${failed} targets"
        exit 1
    } else {
        puts "Test passed for all targets"
        exit 0
    }
}
