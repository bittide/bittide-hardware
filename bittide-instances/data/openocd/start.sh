#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
cd $(dirname $0)
exec openocd-vexriscv -f ports.tcl -f sipeed.tcl -f vexriscv_init.tcl $@
