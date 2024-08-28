#!/bin/bash
# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

# Script that can set up all ethernet interfaces and DHCP on the host machine.
# Based on https://wiki.archlinux.org/title/Network_bridge#With_iproute2

# Name of the bridge
BRIDGE_NAME="eth-fpga"

# File containing the list of network interfaces
INTERFACES_FILE="interfaces"

INTERFACE_IP="10.0.0.1/24"
DHCP_IP="10.0.0.0"

# Function to read interfaces from the file
read_interfaces() {
    if [[ -f "$INTERFACES_FILE" ]]; then
        INTERFACES=($(cat "$INTERFACES_FILE"))
    else
        echo "Error: $INTERFACES_FILE file not found."
        exit 1
    fi
}

# Function to create the bridge
create_bridge() {
    echo "Creating bridge $BRIDGE_NAME..."
    if ip link show $BRIDGE_NAME &> /dev/null; then
        echo "Bridge $BRIDGE_NAME already exists."
    else
        ip link add name $BRIDGE_NAME type bridge
        ip link set dev $BRIDGE_NAME up
        ip addr add $INTERFACE_IP dev $BRIDGE_NAME
    fi
}

# Function to remove the bridge
remove_bridge() {
    echo "Deleting bridge $BRIDGE_NAME..."
    if ip link show $BRIDGE_NAME &> /dev/null; then
        ip link set $BRIDGE_NAME down
        ip link delete $BRIDGE_NAME type bridge
    else
        echo "Bridge $BRIDGE_NAME does not exist."
    fi
}


# Add all ethernet inferfaces specified in $INTERFACES_FILE to the bridge
add_interfaces() {
    for iface in "${INTERFACES[@]}"; do
        echo "Adding interface $iface to bridge $BRIDGE_NAME and setting it up."
        ip link set $iface up
        ip link set $iface master $BRIDGE_NAME
    done
}

# Remove all ethernet inferfaces specified in $INTERFACES_FILE from the bridge
remove_interfaces() {
    for iface in "${INTERFACES[@]}"; do
        echo "Removing interface $iface from bridge $BRIDGE_NAME."
        ip link set $iface nomaster
    done
}

# Check if the bridge and interfaces are set up correctly
# The bridge should exist, be up and have the correct IP address
# All interfaces should exist, be up and have the bridge as master
check_eth() {
    # Check if the bridge exists
    if ! ip link show $BRIDGE_NAME &> /dev/null; then
        echo "Error: Bridge $BRIDGE_NAME does not exist."
        exit 1
    fi
    echo "Bridge $BRIDGE_NAME exists."
    # Check if the bridge is up
    if ! ip link show $BRIDGE_NAME | grep -q "state UP"; then
        echo "Error: Bridge $BRIDGE_NAME is not up."
        exit 1
    fi
    echo "Bridge $BRIDGE_NAME is up."
    # Check if the bridge has the correct IP address
    if ! ip addr show dev $BRIDGE_NAME | grep -q "$INTERFACE_IP"; then
        echo "Error: Bridge $BRIDGE_NAME does not have IP address $INTERFACE_IP."
        exit 1
    fi
    echo "Bridge $BRIDGE_NAME has IP address $INTERFACE_IP."
    # Every interface should be up and should have $BRIDGE_NAME as master
    for iface in "${INTERFACES[@]}"; do
        if ! ip link show $iface &> /dev/null; then
            echo "Error: Interface $iface does not exist."
            exit 1
        fi
        echo "Interface $iface exists."
        if ! ip link show $iface | grep -q "master $BRIDGE_NAME"; then
            echo "Error: Interface $iface is not part of bridge $BRIDGE_NAME."
            exit 1
        fi
        echo "Interface $iface is part of bridge $BRIDGE_NAME."
        if ! ip link show $iface | grep -q "state UP"; then
            echo "Error: Interface $iface is not up."
            exit 1
        fi
        echo "Interface $iface is up."
    done
}

# For dhcp we use `isc-dhcp-server` package
# We need to make sure that the package is installed
# We need to make sure that /etc/default/isc-dhcp-server mentions the bridge as an interface
# We then restart the service and check if it's listening on the correct interface
setup_dhcp() {
    # Make sure the package is installed
    apt-get install isc-dhcp-server

    # Check if $BRIDGE_NAME is in /etc/default/isc-dhcp-server
    # This file should contain INTERFACESv4="$BRIDGE_NAME"
    # If the file contains a different value, throw an error
    # If the file does not exist, create it:
    touch /etc/default/isc-dhcp-server
    if grep -q "INTERFACESv4=\"$BRIDGE_NAME\"" /etc/default/isc-dhcp-server; then
        echo "Bridge $BRIDGE_NAME is already set up for DHCP."
    else
        if grep -q "INTERFACESv4=" /etc/default/isc-dhcp-server; then
            echo "Error: Bridge $BRIDGE_NAME is not set up for DHCP."
            echo "Please set up bridge $BRIDGE_NAME manually in /etc/default/isc-dhcp-server."
            exit 1
        fi
        echo "Setting up bridge $BRIDGE_NAME for DHCP..."
        echo "INTERFACESv4=\"$BRIDGE_NAME\"" >> /etc/default/isc-dhcp-server
    fi

    # When we start the service we should be able to see:
    # Listening on LPF/$BRIDGE_NAME/<MAC ADDRESS>/$DHCP_IP
    systemctl restart isc-dhcp-server

    # Wait for the service to be started
    sleep 1
    result=$(systemctl status isc-dhcp-server | grep "Listening" | grep "$BRIDGE_NAME" | grep "$DHCP_IP")
    if [ -z "$result" ]; then
        echo "Error: isc-dhcp-server is not listening on bridge $BRIDGE_NAME."
        exit 1
    fi
}

# Check if the DHCP server is installed and running
check_dhcp() {
    # Check if the service is installed
    if ! dpkg -l | grep -q "isc-dhcp-server"; then
        echo "Error: isc-dhcp-server is not installed."
        exit 1
    fi
    echo "isc-dhcp-server is installed."

    # Check if the service is running
    if ! systemctl is-active --quiet isc-dhcp-server; then
        echo "Error: isc-dhcp-server is not running."
        exit 1
    fi
    echo "isc-dhcp-server is running."

    #Ideally I'd verify if it's listening on the correct interface
    #But I'm not sure how to do that without restarting the service
}

# Main script logic
case "$1" in
    check)
        read_interfaces
        check_eth
        check_dhcp
        ;;
    dhcp)
        setup_dhcp
        ;;
    eth)
        read_interfaces
        create_bridge
        add_interfaces
        ;;
    rm-eth)
        read_interfaces
        remove_interfaces
        remove_bridge
        ;;
    setup)
        read_interfaces
        create_bridge
        add_interfaces
        setup_dhcp
        ;;
    *)
        echo "Usage: $0 {check|dhcp|eth|rm-eth|setup}"
        echo "  check  - Check if the ethernet bridge $BRIDGE_NAME and DHCP are set up correctly"
        echo "  dhcp   - Install and set up DHCP on the bridge $BRIDGE_NAME, please manually configure /etc/dhcp/dhcpd.conf"
        echo "  eth    - Create an ethernet bridge $BRIDGE_NAME with all interfaces"
        echo "  rm-eth - Delete the ethernet bridge $BRIDGE_NAME"
        echo "  setup  - Perform both eth and dhcp setup"
        exit 1
        ;;
esac
