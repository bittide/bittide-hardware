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

# Default stderr to /dev/null
PICOCOM_STDERR_LOG="${PICOCOM_STDERR_LOG:-/dev/null}"
stderr_dir=$(dirname "${PICOCOM_STDERR_LOG}")
mkdir -p "${stderr_dir}"

picocom  --baud 921600 --imap lfcrlf --omap lfcrlf $@ \
  > >(tee "${PICOCOM_STDOUT_LOG}") \
  2> >(tee "${PICOCOM_STDERR_LOG}" >&2)
