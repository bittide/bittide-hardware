# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

# Tell OpenOCD to use the ftdi interface
adapter driver ftdi

# Find the device based on the USB vendor/product ID
ftdi vid_pid 0x0403 0x6010

set usb_device_loc [env USB_DEVICE]

if { $usb_device_loc == "" } {
  error "Required environment variable 'USB_DEVICE' is not set."
}
adapter usb location $usb_device_loc

# FT2232C IO bits per schematic:
# 0: TCK, Output
# 1: TDO, Output
# 2: TDO, Input
# 3: TMS, Output
# 4: Not connected
# 5: RST, Output
#
# The first 16bit value is the initial IO state. Just make TMS and RST high
# The second 16bit value is data direction for each pin, 1 = Output
ftdi layout_init 0x0028 0x2b

# We'll use RST for the system reset, not the JTAG reset. Thus, disable nTRST
# by setting data and enable mask to 0. If you want to use
# the RST pin for nTRST instead, switch this and the nSRST line.
ftdi layout_signal nTRST -data 0x0 -oe 0x0

# RST is on bit 5, so the mask is 0x20. The pin is directly connected, so
# we don't have an output-enable pin -> set the same mask
ftdi layout_signal nSRST -data 0x0020 -oe 0x0020

# The FT2232C supports sustained data rates up to 5.6 Mbit/s
# https://ftdichip.com/wp-content/uploads/2020/08/DS_FT2232C.pdf
# Just to be safe we set it to 1000 kHz
adapter speed 1000
transport select jtag
