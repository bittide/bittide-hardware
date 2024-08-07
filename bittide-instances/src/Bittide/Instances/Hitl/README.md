<!--
SPDX-FileCopyrightText: 2022 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# Hardware in the loop tests
Besides simulation, we also want to test our designs on physical hardware. We
have constructed a demo rig, which consists of 8 FPGA boards (KCU105), which are
all connected to a PC through their JTAG ports. This PC runs a GitHub runner.

To add a HTIL test:

- Instantiate `Clash.Hitl.hitlVio` in your design
- Add your test to `hitlTests` in ([Tests.hs](/bittide-instances/src/Bittide/Instances/Hitl/Tests.hs))
- Add your test to `targets` in ([Shake.hs](/bittide-shake/bin/Shake.hs))
- Add your test to CI:
  - [staging](/.github/synthesis/staging.json) runs on every PR,
    [all](/.github/synthesis/all.json) runs every night
  - Set stage to `test`

A design marked as a Hardware-in-the-loop test (hitlt) should adhere to
framework described in the next chapters.

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

The default ILA configuration (`ilaConfig`, see [Clash.Cores.Xilinx.Ila](https://github.com/clash-lang/clash-compiler/blob/master/clash-cores/src/Clash/Cores/Xilinx/Ila.hs#L63) is valid
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


## Post processing of ILA data
If a Shake target has a post processing function, this is executed after the
hardware test as part of the `:test` call. The post processing function can also
be called without performing the hardware test using `:post-process`.

To add post processing to a bittide instance:

1. Create a Haskell file in `bittide-instances/bin/Post/` with a `main`
function. This file can import any file from `Bittide.Instances`. The function
is called from Shake with 2 arguments: filepath of the ILA data directory and
the exit code of the hardware test which generated the ILA data.
2. Add an executable in `bittide-instances.cabal` for the new Haskell file.
3. In `Shake.hs`, add a `Target` for the instance, and set `targetPostProcess`
to the name of the executable created in the step above.

See the example for the instance `boardTestExtended`.
