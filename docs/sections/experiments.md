<!--
SPDX-FileCopyrightText: 2025 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# Experiments
This chapter describes the existing infrastructure to support the creation and execution of experiments on the example Bittide system.

## Required components
- **Example design**: Contains all bittide related FPGA logic.
- **Programs**: The binary files that will be loaded into the CPUs that exist in the example design.
- **Driver**: Runs on the host PC and communicates with the testbench.
- **Testbench**: Contains the example design and other necessary logic such as ILA's.

## Relevant infrastructure
Names which tools are relevant and where they live

## Execution
Describes how tests are executed as part of a CI/CD pipeline.

## Writing programs
Describes how to write new programs for the management unit, general purpose processing element or clock control.

## Writing a driver
Describes how to write a driver for the experiment.

## Adding your experiment to CI/CD
Describes how to add your experiment to the CI/CD pipeline.
