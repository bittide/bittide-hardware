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

# Timeout specifying how long we should wait for a test to finish before
# considering it a failed test.
set test_timeout_ms 60000

# Timeout specifying how long to wait for hardware targets (FPGAs) to become
# available in the hardware server.
set hw_server_timeout_ms 5000

# Prefix of the name of a VIO probe.
set vio_prefix ""

# The VIO probes used for hardware-in-the-loop tests (hitlt) must end their
# prefix with 'vioHitlt'. For example, a probe named 'my_vio_vioHitlt/probe_test_done'
# has the prefix 'my_vio_vioHitlt'. Throws an error when not exactly 1 VIO core
# is present.
proc set_vio_prefix {} {
    global vio_prefix

    # Use `probe_test_done` as the probe to find full probe names
    set probe_done [get_hw_probes *vioHitlt/probe_test_done]
    if {[expr [llength $probe_done] != 1]} {
        puts "Exactly 1 VIO core with the prefix '*vioHitlt' must be present"
        exit 1
    }
    set vio_prefix [lindex [split [get_property name $probe_done] "/"] 0]
}

# For the Hardware-in-the-Loop test (hitlt) at least 3 specific probes need to
# be present in the design:
# - `probe_test_done` indicates when a single test is done
# - `probe_test_success` indicates whether a single test was successful
# - `probe_test_start*` indicate the start of a specific test
# Other VIO probes may be present in the design, but are only used to print
# debug information when a test fails.
proc verify_vio_probes {} {
    global vio_prefix

    set done_probe [get_hw_probes ${vio_prefix}/probe_test_done]
    if {[expr [llength $done_probe] != 1]} {
        set done_probe_count [llength $done_probe]
        puts "Exactly one probe named '$vio_prefix/probe_test_done' must be present, but ${done_probe_count} were found"
        print_all_probe_names
        exit 1
    } elseif {[expr {[get_property type $done_probe] != "vio_input"}]} {
        set probe_name [get_property name.short $done_probe]
        puts "Probe '${probe_name}' must have type 'vio_input'"
        print_all_probe_names
        exit 1
    } elseif {[expr {[get_property width $done_probe] != 1}]} {
        set probe_name [get_property name.short $done_probe]
        puts "Probe '${probe_name}' must have a width of 1 bit"
        print_all_probe_names
        exit 1
    }

    set success_probe [get_hw_probes ${vio_prefix}/probe_test_success]
    if {[expr [llength $success_probe] != 1]} {
        set success_probe_count [llength $success_probe]
        puts "Exactly one probe named '$vio_prefix/probe_test_success' must be present, but ${success_probe_count} were found"
        print_all_probe_names
        exit 1
    }
    if {[expr {[get_property type $success_probe] != "vio_input"}]} {
        set probe_name [get_property name.short $success_probe]
        puts "Probe '${probe_name}' must have type 'vio_input'"
        print_all_probe_names
        exit 1
    } elseif {[expr {[get_property width $success_probe] != 1}]} {
        set probe_name [get_property name.short $success_probe]
        puts "Probe '${probe_name}' must have a width of 1 bit"
        print_all_probe_names
        exit 1
    }

    set start_probes [get_hw_probes ${vio_prefix}/probe_test_start*]
    if {[expr [llength $start_probes] < 1]} {
        set start_probe_count [llength $start_probes]
        puts "At least one probe named '$vio_prefix/probe_test_start*' must be present, but none were found"
        print_all_probe_names
        exit 1
    }
    foreach probe $start_probes {
        if {[expr {[get_property type $probe] != "vio_output"}]} {
            set probe_name [get_property name.short $probe]
            puts "Probe '$probe_name' must have type 'vio_output'"
            print_all_probe_names
            exit 1
        } elseif {[expr {[get_property width $probe] != 1}]} {
            set probe_name [get_property name.short $probe]
            puts "Probe '$probe_name' must have a width of 1 bit"
            print_all_probe_names
            exit 1
        }
    }
}

# Create a list of dictionaries where each dictionary corresponds to one ILA.
# Each dictionary has the following keys:
#   name          : short name of the ILA
#   cell_name     : name of the cell the ILA is in
#   trigger_probe : name of the trigger probe
#   capture_probe : name of the capture probe
#   data_probes   : list of names of all other probes
proc get_ila_dicts {} {
    set ila_dicts [list]

    set hw_ilas [get_hw_ilas -quiet]
    set ila_count [llength $hw_ilas]
    if {[expr $ila_count == 0]} {
        puts "\nNo ILAs in design"
        return [list]
    }

    puts "\nFound $ila_count ILAs:"
    foreach hw_ila $hw_ilas {
        set ila_dict [dict create]

        set cell_name [get_property CELL_NAME $hw_ila]
        set idx_start [expr [string first "_" $cell_name] + 1]
        set idx_end [expr [string first "/" $cell_name] - 1]
        set short_name [string range $cell_name $idx_start $idx_end]
        dict set ila_dict name $short_name
        dict set ila_dict cell_name $cell_name

        # Get trigger probe and verify it conforms with ILA framework
        set trigger_probe [get_hw_probes -of_objects $hw_ila */trigger*]
        set trigger_probe_count [llength $trigger_probe]
        if {[expr {$trigger_probe_count != 1}]} {
            puts "Exactly one probe named 'trigger*' must be present, but $trigger_probe_count were found"
            print_all_probe_names
            exit 1
        } elseif {[expr {[get_property is_trigger $trigger_probe] != 1}]} {
            set probe_name_short [get_property name.short $trigger_probe]
            puts "Probe '$probe_name_short' should have probeType Trigger or DataAndTrigger"
            print_all_probe_names
            exit 1
        } elseif {[expr {[get_property width $trigger_probe] != 1}]} {
            set probe_name_short [get_property name.short $trigger_probe]
            puts "Probe '$probe_name_short' must have a width of 1 bit"
            print_all_probe_names
            exit 1
        } else {
            dict set ila_dict trigger_probe [get_property name $trigger_probe]
        }

        # Get capture probe and verify it conforms with ILA framework
        set capture_probe [get_hw_probes -of_objects $hw_ila */capture*]
        set capture_probe_count [llength $capture_probe]
        if {[expr {$capture_probe_count != 1}]} {
            puts "Exactly one probe named 'capture*' must be present, but $capture_probe_count were found"
            print_all_probe_names
            exit 1
        } elseif {[expr {[get_property is_trigger $capture_probe] != 1}]} {
            set probe_name_short [get_property name.short $capture_probe]
            puts "Probe '$probe_name_short' should have probeType Trigger or DataAndTrigger"
            print_all_probe_names
            exit 1
        } elseif {[expr {[get_property width $capture_probe] != 1}]} {
            set probe_name_short [get_property name.short $capture_probe]
            puts "Probe '$probe_name_short' must have a width of 1 bit"
            print_all_probe_names
            exit 1
        } else {
            dict set ila_dict capture_probe [get_property name $capture_probe]
        }

        # Get all data probes and verify each conforms with ILA framework
        set all_probes [get_hw_probes -of_objects $hw_ila]
        if {[expr {[llength $all_probes] < 3}]} {
            puts "ILA '$short_name' has no data probes, at least 1 data probe is required"
            print_all_probe_names
            exit 1
        }
        dict set ila_dict data_probes [list]
        foreach probe $all_probes {
            if {[expr {$probe == $trigger_probe} || {$probe == $capture_probe}]} {
                continue
            } elseif {[expr {[get_property is_data $probe] != 1}]} {
                set probe_name_short [get_property name.short $probe]
                puts "Probe '$probe_name_short' should have probeType Data or DataAndTrigger"
                print_all_probe_names
                exit 1
            } else {
                dict update ila_dict data_probes probe_list {
                    lappend probe_list [get_property name $probe]
                }
            }
        }
        lappend ila_dicts $ila_dict

        # Print all ILA probes
        puts "ILA $short_name with probes:"
        set probe_name_short [get_property name.short $trigger_probe]
        puts "\t$probe_name_short"
        set probe_name_short [get_property name.short $capture_probe]
        puts "\t$probe_name_short"
        foreach probe_name [dict get $ila_dict data_probes] {
            set idx_start [expr [string first "/" $probe_name] + 1]
            set probe_name_short [string range $probe_name $idx_start end]
            puts "\t$probe_name_short"
        }
    }
    return $ila_dicts
}

proc print_all_probe_names {} {
    set probes [get_hw_probes]
    puts "All probes in design:"
    foreach probe $probes {
        set probe_name [get_property name $probe]
        puts "\t${probe_name}"
    }
}

proc get_part_name {url id} {
    return ${url}/xilinx_tcf/Digilent/${id}
}

# Creates an ordered dictionary which maps indices of FPGAs in the demo rack to
# their respecive FPGA IDs. If an empty list of fpga_nrs is given, the FPGA ID
# of the first hardware target is given (this can be any FPGA).
proc get_target_dict {url fpga_nrs} {
    global fpga_ids
    set target_dict [dict create]
    if {[expr [llength $fpga_nrs] == 0]} {
        set fpga_nrs [list -1]
    }
    foreach fpga_nr $fpga_nrs {
        if {$fpga_nr == -1} {
            set target_name [lindex [get_hw_targets] 0]
            set target_id [lindex [split $target_name /] 3]
        } else {
            set target_id [lindex $fpga_ids $fpga_nr]
        }
        dict set target_dict $fpga_nr $target_id
    }
    return $target_dict
}


# Prints all VIOs in the radix they are set. A current hardware device must be
# set before calling this function. Probes are grouped by VIO.
proc print_all_vios {} {
    set probes [get_hw_probes -of_objects [get_hw_vios]]

    # Find the maximum widths of each column, with a minimum of the header length
    set w_name 4
    set w_value 5
    set w_radix 5
    foreach probe $probes {
        set type [get_property type $probe]
        set w_name_cur [string length [get_property name.short $probe]]
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

    foreach vio [get_hw_vios] {
        set input_probes [get_hw_probes -of_objects $vio -filter {type == vio_input} -quiet]
        foreach input_probe $input_probes {
            set name [get_property name.short $input_probe]
            set value [get_property input_value $input_probe]
            set radix [get_property input_value_radix $input_probe]
            puts [format "| %-*s | %*s | %-*s |" $w_name $name $w_value $value $w_radix $radix]
        }
        puts $sep

        set output_probes [get_hw_probes -of_objects $vio -filter {type == vio_output} -quiet]
        foreach output_probe $output_probes {
            set name [get_property name.short $output_probe]
            set value [get_property output_value $output_probe]
            set radix [get_property output_value_radix $output_probe]
            puts [format "| %-*s | %*s | %-*s |" $w_name $name $w_value $value $w_radix $radix]
        }
        puts $sep
    }
}

# Return all values in lista, which are not listb.
proc difference {lista listb} {
    set A {}
    foreach a $lista {
        dict set A $a 0
    }
    foreach b $listb {
        dict unset A $b
    }
    return [dict keys $A]
}

# Return the intersection of two lists. Note that this functions complexity is
# O(n^2), and should not be used for big lists.
proc intersection {lista listb} {
    set intersect [list]
    foreach a $lista {
        if {$a in $listb} {
            lappend intersect $a
        }
    }
    return $intersect
}

# Checks whether the expected hardware targets are connected, if not exit.
proc has_expected_targets {url expected_target_dict} {
    set expected_names {}
    dict for {nr id} $expected_target_dict {
        lappend expected_names [get_part_name $url $id]
    }
    set expected_count [dict size $expected_target_dict]

    set start_time [clock milliseconds]
    set i 0
    while 1 {
        # Check if expected hardware targets are connected
        set hw_targets [get_hw_targets -quiet]
        set hw_target_count [llength $hw_targets]
        set found_targets [intersection $expected_names $hw_targets]
        set found_targets_count [llength $found_targets]
        if {[expr {$found_targets_count == $expected_count}]} {
            puts "Hardware server at ${url} hosts ${hw_target_count} hardware targets:"
            foreach hw_target $hw_targets {
                puts "\t$hw_target"
            }
            puts ""
            break
        }

        # Timeout if test takes longer than `hw_server_timeout_ms`
        global hw_server_timeout_ms
        set current_time [clock milliseconds]
        set time_spent [expr {$current_time - $start_time}]
        if {${time_spent} > ${hw_server_timeout_ms}} {
            puts "Expected hardware targets:"
            dict for {nr id} $expected_target_dict {
                set tgt [get_part_name $url $id]
                if {[expr {[lsearch -exact $hw_targets $tgt] == -1}]} {
                    set not_found "<- not found"
                } else {
                    set not_found ""
                }
                puts "$tgt - FPGA $nr $not_found"
            }
            set unexpected_targets [difference $hw_targets $expected_names]
            if {[expr [llength $unexpected_targets] > 0]} {
                puts "Hardware targets which are not expected:"
                foreach tgt $unexpected_targets {
                    puts $tgt
                }
            }
            exit 1
        }

        puts "Attempt ${i} : Found ${found_targets_count} out of expected ${expected_count} hardware targets"
        incr i
        after 500
        refresh_hw_server
    }
}

# Set the target board as the current hardware target and return its device
proc load_target_device {target_name} {
    if {[expr {$target_name != [get_property NAME [current_hw_target]]}]} {
        close_hw_target
        current_hw_target [get_hw_targets $target_name]
    }
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

# Verify that `done` is not set before starting the test
proc verify_before_start {start_probe} {
    global vio_prefix
    set_property OUTPUT_VALUE 0 $start_probe
    commit_hw_vio [get_hw_vios]
    refresh_hw_vio [get_hw_vios]
    set done [get_property INPUT_VALUE [get_hw_probes ${vio_prefix}/probe_test_done]]
    if {$done != 0} {
        puts "\tERROR: test is done before starting the test"
        print_all_vios
        exit 1
    }
}

# Refresh the input probes until the done flag is set. Retries for up to
# `test_timeout_ms` milliseconds, counting from a given `start_time`.
proc wait_test_end {start_time} {
    global vio_prefix
    while 1 {
        # Check test status, break if test is done
        refresh_hw_vio [get_hw_vios]
        set done [get_property INPUT_VALUE [get_hw_probes ${vio_prefix}/probe_test_done]]
        set success [get_property INPUT_VALUE [get_hw_probes ${vio_prefix}/probe_test_success]]
        if {$done == 1} {
            break
        }

        # Timeout if test takes longer than `test_timeout_ms`
        global test_timeout_ms
        set current_time [clock milliseconds]
        set time_spent [expr {$current_time - $start_time}]
        if {${time_spent} > ${test_timeout_ms}} {
            break
        }
    }
    set end_time [clock milliseconds]
    return [list $done $success $start_time $end_time]
}

# Print test results. Prints all VIO probes when a test fails
proc print_test_results {done success start_time end_time} {
    if {$done == 0} {
        global test_timeout_ms
        puts "\tTest timeout: done flag not set after ${test_timeout_ms} ms"
        set timestamp_start [format_time $start_time]
        puts "\tStarted test: $timestamp_start"
        set timestamp_end [format_time $end_time]
        puts "\tEnded test:   $timestamp_end"
        print_all_vios
    } elseif {$success == 0} {
        puts "\tTest failed"
        print_all_vios
    } else {
        puts "\tTest passed"
    }
}

# Gets the short names of probes which contain 'probe_test_start'
proc get_test_names {} {
    global vio_prefix
    set start_probes [get_hw_probes ${vio_prefix}/probe_test_start*]
    if {[expr [llength $start_probes] == 0]} {
        puts "No probes found with name '*probe_test_start*', which are needed to start tests"
        exit 1
    }
    set start_probe_names {}
    foreach start_probe $start_probes {
        lappend start_probe_names [get_property name.short $start_probe]
    }
    return $start_probe_names
}

proc run_test_group {probes_file target_dict url ila_data_dir} {
    # Load the device of the first target
    set target_id [lindex [dict values $target_dict] 0]
    set target_name [get_part_name $url $target_id]
    set device [load_target_device $target_name]
    set_property PROBES.FILE ${probes_file} $device
    refresh_hw_device $device

    # Set the prefix of VIO probes and verify all required probes are present.
    set_vio_prefix
    verify_vio_probes
    global vio_prefix

    set ila_dicts [get_ila_dicts]

    set successful_tests 0

    set target_count [dict size $target_dict]

    # Get all the test names
    set start_probe_names [get_test_names]
    set test_count [llength $start_probe_names]
    puts "\nFound ${test_count} tests:"
    foreach start_probe_name $start_probe_names {
        puts "\t${start_probe_name}"
    }

    foreach start_probe_name $start_probe_names {
        set successful_targets 0
        puts "\nRunning test: $start_probe_name"

        # Verify pre-start condition and start test
        dict for {target_nr target_id} $target_dict {
            # Load device
            set device [load_target_device [get_part_name $url $target_id]]
            set_property PROBES.FILE ${probes_file} $device
            refresh_hw_device $device -quiet
            # Verify pre-start condition
            set start_probe [get_hw_probes ${vio_prefix}/${start_probe_name}]
            verify_before_start $start_probe

            # Activate the trigger for each ILA
            foreach ila_dict $ila_dicts {
                set cell_name [dict get $ila_dict cell_name]
                set ila [get_hw_ilas -filter CELL_NAME=={${cell_name}}]
                # Set trigger probe (active high boolean)
                set trigger_probe [get_hw_probes [dict get $ila_dict trigger_probe]]
                set_property trigger_compare_value eq1'b1 $trigger_probe

                # Enable capture control and set capture probe (active high boolean)
                set_property control.capture_mode BASIC $ila
                set capture_probe [get_hw_probes [dict get $ila_dict capture_probe]]
                set_property capture_compare_value eq1'b1 $capture_probe

                # Set the trigger position
                set_property control.trigger_position 0 $ila

                run_hw_ila $ila
            }

            # Start the test
            set_property OUTPUT_VALUE 1 $start_probe
            commit_hw_vio [get_hw_vios]

            puts "Start test for FPGA ${target_nr} with ID ${target_id}"
        }

        puts "\nWaiting on test end: $start_probe_name"
        set start_time [clock milliseconds]
        dict for {target_nr target_id} $target_dict {
            # Load device
            set device [load_target_device [get_part_name $url $target_id]]
            set_property PROBES.FILE ${probes_file} $device
            refresh_hw_device $device -quiet
            # Wait for the test to end
            set test_results [wait_test_end ${start_time}]
            lassign $test_results done success start_time end_time
            # Print test results of this FPGA
            puts "\tTested for FPGA ${target_nr} with ID ${target_id}"
            print_test_results $done $success $start_time $end_time
            if {[expr $done == 1] && [expr $success == 1]} {
                incr successful_targets
            }
        }

        puts "\nStopping test: $start_probe_name"
        dict for {target_nr target_id} $target_dict {
            # Load device
            set device [load_target_device [get_part_name $url $target_id]]
            set_property PROBES.FILE ${probes_file} $device
            refresh_hw_device $device -quiet

            # Load the ILA data from the FPGA
            foreach ila_dict $ila_dicts {
                # Create the directory, if it does not exist already
                if {[expr {$target_nr < 0}]} {
                    set index_id "X_${target_id}"
                } else {
                    set index_id "${target_nr}_${target_id}"
                }
                set dir [file join $ila_data_dir $start_probe_name $index_id]
                file mkdir $dir
                set ila_name [dict get $ila_dict name]
                set file_path [file join $dir "$ila_name"]

                set cell_name [dict get $ila_dict cell_name]
                set ila [get_hw_ilas -filter CELL_NAME=={${cell_name}}]

                set ila_data [upload_hw_ila_data $ila]
                write_hw_ila_data -force -csv_file $file_path $ila_data
                write_hw_ila_data -force -vcd_file $file_path $ila_data
            }

            # Reset the start probe for the current test
            set start_probe [get_hw_probes ${vio_prefix}/${start_probe_name}]
            set_property OUTPUT_VALUE 0 $start_probe
            commit_hw_vio [get_hw_vios]
        }
        # Print summary of individual test
        puts "\nTest ${start_probe_name} passed on ${successful_targets} out of ${target_count} targets"
        if {[expr $successful_targets == $target_count]} {
            incr successful_tests
        }
    }

    # Print summary of all tests
    if {[expr $successful_tests == $test_count]} {
        puts "\nAll ${successful_tests} tests passed on ${target_count} targets"
        exit 0
    } else {
        set failed_tests [expr ${test_count} - ${successful_tests}]
        puts "\nFailed for ${failed_tests}/${test_count} tests"
        exit 1
    }
}
