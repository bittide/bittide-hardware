<!--
SPDX-FileCopyrightText: 2022-2024 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# Hardware in the loop tests
Besides simulation, we also want to test our designs on physical hardware. We
have constructed a demo rig, which consists of 8 FPGA boards (KCU105), which are
all connected to a PC through their JTAG ports. This PC runs a GitHub runner.

To run a hardware-in-the-loop test:
- The design must include a HITLT VIO (see [VIO](#vio))
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
Use the `vioHitlt` interface of
[Bittide.Instances.Hitl](/bittide-instances/src/Bittide/Instances/Hitl.hs)
to instantiate the VIO. Each HITLT VIO can be associated a unique type
interface for test case selection and passing configuration data to the
selected test. This component offers at least 3 probes.

2 input probes:
- `probe_test_done` indicates when the currently selected test is done
- `probe_test_success` indicates whether the currently selected test was
  successful

1 output probe:
- `probe_test_start` indicates the start of a specific test case and passes
  configuration data to the test

The `done` signal should be `0` before a test is started. The `success` signal
should only be read when `done` is `1`, otherwise its value is not defined.
More input probes can be added to the vio for debugging purposes, but they
won't affect the execution of the tests.

The output probe controls the test activation, selects the test case and
passes configuration data to the test. It offers values of type
`Maybe (Index n, a)` holding a `Just` value only during test activation. The
currently active test case is selected via the first component, where `n`
is the number of test cases in total. The second component offers the
corresponding configuration  associated with the test. The polymorphic type
`a` is associated with the test through a decitated test interface:

If only a single test case is required, the pre-defined `SimpleTest` type should
be used. It does neither require any particular test name nor a dedicated YAML
configuration file.

```haskell
myHitltVio = vioHitlt @SimpleTest ...
```

If multiple test cases are required, but no configuration data needs to be
passed to the test, the `SimpleTests` type should be used. The names of the test
cases are proveded as a symbol list.

```haskell
myHitltVio = vioHitlt @(SimpleTests '["test1", "test2", "test3"]) ...
```

If dedicated configuration data of type `a` needs to be passed to one or multiple
test cases, an class instance of the `HitltConfig` class must be declared. The
instance also associates the type with all test case names of the test. See
[Bittide.Instances.Hitl.FincFdec](/bittide-instances/src/Bittide/Instances/Hitl/FincFdec.hs)
for an example. If a custom type is needed to pass additional configuration data
to a test, then this type must associated with a corresponding configuration file
name in [YamlPack.hs](/bittide-instances/bin/YamlPack.hs).

Test activity and configuration data for particular test cases can be selected
via the `testActive` and `testConfig` helpers using the name of the particular
test case to be selected as a type level symbol argument.

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
