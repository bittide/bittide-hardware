<!--
SPDX-FileCopyrightText: 2024 Google LLC

SPDX-License-Identifier: Apache-2.0
-->
This folder contains all information and scripts to bootstrap the HITL setup.
Setting up should be done manually, CI should only check if the setup is correct.

The setup script does the following:
* Create a bridge device
* Connect all interfaces defined in the `interfaces` file to the bridge
* Set up the bridge device with a static IP address
* Set up DHCP server for the bridge device
