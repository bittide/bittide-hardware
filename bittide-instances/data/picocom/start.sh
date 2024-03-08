#!/usr/bin/env bash
#
# Start picocom with defaults suitable for our instances. Pass in a USB device,
# for example:
#
#    /dev/serial/by-id/usb-FTDI_Dual_RS232-if01-port0
#
picocom  --baud 921600 --imap lfcrlf --omap lfcrlf $@
