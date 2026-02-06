#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
#
# Start picocom with defaults suitable for our instances. Pass in a USB device,
# for example:
#
#    /dev/serial/by-id/usb-FTDI_Dual_RS232-if01-port0
#
set -e

# Default stdout to /dev/null
PICOCOM_STDOUT_LOG="${PICOCOM_STDOUT_LOG:-/dev/null}"
stdout_dir=$(dirname "${PICOCOM_STDOUT_LOG}")
mkdir -p "${stdout_dir}"
touch "$PICOCOM_STDOUT_LOG"

# Default stderr to /dev/null
PICOCOM_STDERR_LOG="${PICOCOM_STDERR_LOG:-/dev/null}"
stderr_dir=$(dirname "${PICOCOM_STDERR_LOG}")
mkdir -p "${stderr_dir}"
touch "$PICOCOM_STDERR_LOG"

PICOCOM_BAUD="${PICOCOM_BAUD:-921600}"

# Async read from the picocom logs, so a busy downstream process does not cause
# picocom to drop log data
# Flags: --pid: causes tail to close when picocom closes
#        -n 0: Only read out the latest lines, not the last 10 lines
#        -F: Could also be -f, but -F is apparently slightly safer due to inodes
# tail --pid=$$ -n 0 -F "${PICOCOM_STDERR_LOG}" >&2 &

picocom --baud "${PICOCOM_BAUD}" --imap lfcrlf --omap lfcrlf --exit-after 10000 $@ \
  > "${PICOCOM_STDOUT_LOG}" &

sleep 1 ; tail -n +1 -F "${PICOCOM_STDOUT_LOG}" | tee "${stdout_dir}/myTest.log"
