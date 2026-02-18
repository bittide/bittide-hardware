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
PICOCOM_STDOUT_LOG="${PICOCOM_STDOUT_LOG:?PICOCOM_STDOUT_LOG should be set}"
stdout_dir=$(dirname "${PICOCOM_STDOUT_LOG}")
mkdir -p "${stdout_dir}"
rm -f "$PICOCOM_STDOUT_LOG" ; touch "$PICOCOM_STDOUT_LOG"

# Default stderr to /dev/null
PICOCOM_STDERR_LOG="${PICOCOM_STDERR_LOG:?PICOCOM_STDERR_LOG should be set}"
stderr_dir=$(dirname "${PICOCOM_STDERR_LOG}")
mkdir -p "${stderr_dir}"
rm -f "$PICOCOM_STDERR_LOG" ; touch "$PICOCOM_STDERR_LOG"

PICOCOM_BAUD="${PICOCOM_BAUD:-921600}"
# Since picocom times out after not receiving input, define the number of ms
# to wait for new input before timing out picocom
PICOCOM_TIMEOUT="${PICOCOM_TIMEOUT:-$((2**32))}"

picocom --baud "${PICOCOM_BAUD}" --imap lfcrlf --omap lfcrlf --exit-after "${PICOCOM_TIMEOUT}" $@ \
  >> "${PICOCOM_STDOUT_LOG}" \
  2>> "${PICOCOM_STDERR_LOG}" &

tail -n +1 -f "${PICOCOM_STDOUT_LOG}"
