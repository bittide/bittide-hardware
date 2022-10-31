#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
set -euo pipefail
IFS=$'\n\t'

# Get Vivado environment in scope
source /opt/tools/Xilinx/VivadoEnterprise/Vivado/2022.1/settings64.sh

# Work around https://support.xilinx.com/s/question/0D52E000079NURRSA4/synthesis-failed-abnormal-termination-tcmalloc-large-allocation?language=en_US
export LD_PRELOAD=/lib/x86_64-linux-gnu/libudev.so.1

# Vivado insists on using en_US.UTF-8
apt-get update
apt-get install language-pack-en-base -y
locale-gen en_US.UTF-8
export LC_ALL=en_US.UTF-8
update-locale

# Vivado dependencies
apt-get install libtinfo5 libncurses5 libx11-6 -y

# Run command given as argument
$@
