<!--
SPDX-FileCopyrightText: 2022-2023 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# Hardware in the loop tests
Besides simulation, we also want to test our designs on physical hardware. We
have constructed a demo rig, which consists of 8 FPGA boards (KCU105), which are
all connected to a PC through their JTAG ports. This PC runs a GitHub runner.

To run a hardware-in-the-loop test:
- The design must include a VIO (see [VIO](#vio))
- Add topentity to list of targets ([Shake.hs](/bittide-shake/bin/Shake.hs))
- Add topentity to CI
    - [staging](/.github/synthesis/staging.json) runs on every PR,
    [all](/.github/synthesis/all.json) runs every night
    - Stage is `test`
    - Targets can be `All`, `OneAny` or `Specific [{list of indices in rig}]`
    see [Flags.hs](/bittide-shake/src/Clash/Shake/Flags.hs)

A design marked as a Hardware-in-the-loop test (hitlt) should adhere to
framework described in the next chapters.


## VIO
The VIO component should be called `vioHitlt`, this name may include a module
prefix (e.g. `fincFdecTest30_vioHitlt`). The name of the component can be set as
such:

```haskell
myVio = setName @"vioHitlt" $ vioProbe ...
```

This component must have at least 3 probes. More start probes can be added,
where each start probe defines a single test. The tests are executed one-by-one,
in the order they are given to the `vioProbe` function.

2 input probes:
- `probe_test_done` indicates when a single test is done
- `probe_test_success` indicates whether a single test was successful

1 output probe:
- `probe_test_start*` indicate the start of a specific test

The `done` signal should be `0` before a test is started. The `success` signal
should only be read when `done` is `1`, otherwise its value is not defined.


## ILA
All ILAs present in the design are armed before the VIO test is started. Each
ILA produces a separate CSV file for each VIO test.

For the Hardware-in-the-Loop test (hitlt) at least 3 probes need to be present
in the design:
- `trigger*` is an active-high boolean value (`probeType` is `Trigger` or
`DataAndTrigger`). This signal _may_ be sticky, but a single cycle is enough.
- `capture*` indicates when to sample data values (`probeType` is `Trigger` or
`DataAndTrigger`)
- At least one data probe. All other probes must have `probeType` `Data` or
`DataAndTrigger`.

The CSV files are written to the following directory:

```
_build/vivado/{instance}/ila-data/{start_probe_name}/{index in rig}_{FPGA id}
```

In this directory, a CSV file with the name of the ILA is written. The name of
the ILA can be set with `setName`, identical to setting a name for a VIO. Note
that a lot of CSV files can be generated, e.g. a hardware-in-the-loop test with
2 start probes and 2 ILAs programmed on all 8 FPGAs in the demo rig results in
32 CSV files.

The default ILA configuration (`ilaConfig`, see Clash.Cores.Xilinx.Ila) is valid
for hardware-in-the-loop tests. If a custom configuration is used, make sure to
set `captureControl` to `True`, and use the `probeType`s described above.

All ILA data is uploaded from the FPGA to the PC after the VIO test is finished
(or timed out). If an ILA did not trigger, the saved CSV file will only contain
the header.


## Pseudo-code of a hardware-in-the-loop test
A complete hardware-in-the-loop test as pseudo-code:
```
for each FPGA
    upload bitstream to FPGA
for each test
    for each FPGA
        assert `probe_test_done` is `0`
        arm all ILAs
        start test by setting `probe_test_start_x` to `1`
    for each FPGA
        wait for `probe_test_done` to assert
        print test results
        if test failed
            print all VIO probe values
    for each FPGA
        upload ILA data
        stop test by setting `probe_test_start_x` to `0`
    print test summary
print summary all tests
```
