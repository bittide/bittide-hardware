<!--
SPDX-FileCopyrightText: 2022-2023 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# HITLT YAML Configurations

## User configurations

The user HITLT YAML configurations must be structured as follows:

```yaml
testname1:
  default: <cfg>  # overwrites the default configuration value for this test case
  disable: true # disables the test case (default: false)
  fpga:
    0:              # first fpga according to Bittide.Instances.Hitl.Setup
      disable: true # disables the test execution for this fpga (default: false)
      data: <cfg>     # sets a new configuration value for this fpga only
    1:              # second fpga according to Bittide.Instances.Hitl.Setup
      ...
testname2:
  ...
```

where
  * `testname1`, `testname2`, ... are the test names associated with test cases,
  * `<cfg>` is a YAML sub-tree according to the `FromJSON` instance of the
    type associated with the test,
  * all entries are optional (defaults are used for every entry not being
    present), and
  * disabling a test case turns the remaining sub-tree obsolete.

All files in this folder are recognized by the `yamlpack` tool automatically,
which fixes the mapping to the associated types for each test. Also see the
[HITLT README](/bittide-instances/src/Bittide/Instances/Hitl/README.md) for more
details on that.

## Packed configurations

The `yamlpack` tool can be used to generate a more condense version of the
configurations to be read via the TCL interface, where the user written
configurations in this folder are used to overwrite the defaults used for
generating these packed variants.

The packed HITLT YAML configurations are structured as follows:

```yaml
testname1:
  <id0>: <binary data>
  <id1>: <binary data>
  ...
testname2:
  ...
```

where
  * `testname1`, `testname2`, ... are the test names associated with each test case,
  * disabled test cases are not listed in the packed configurations,
  * `<id0>`, `<id1>`, ... are the FPGA ids according to
    [Bittide.Instances.Hitl.Setup](/bittide-instances/src/Bittide/Instances/Hitl/Setup.hs),
  * `<binary data>` is a string representing the binary data to be passed to the
    test via the VIO.
