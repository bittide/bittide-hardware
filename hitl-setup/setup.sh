#!/bin/bash
# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

FORCE="$1"
set -xuo pipefail
HERE=$(realpath "$(dirname "$0")")
source "$HERE/fpganet" || exit $?

echo "Checking that all INTERFACES exist"
for INTERFACE in $INTERFACES; do
  ip addr show "$INTERFACE" > /dev/null || exit $?
done
echo "All INTERFACES exist"

FILES="/etc/systemd/system/fpganet.service /etc/fpganet /etc/dhcp/dhcpd.conf /etc/default/isc-dhcp-server"
EXISTS=
for FILE in $FILES; do
  if [ -e "$FILE" ]; then
    EXISTS="$EXISTS $FILE"
  fi
done
if [ -n "$EXISTS" ] && [ "$FORCE" != -f ]; then
  set +x
  echo "Running this script with -f will overwrite:"
  echo "$EXISTS"
  echo "But now it will stop, because -f wasn't supplied"
  exit 1
fi

set -e
apt-get install -y isc-dhcp-server iproute2

cp "$HERE/fpganet.service" /etc/systemd/system
cp "$HERE/fpganet" /etc

cp "$HERE/dhcpd.conf" /etc/dhcp/
echo "INTERFACESv4=\"$BRIDGE_NAME\"" > /etc/default/isc-dhcp-server

systemctl daemon-reload

systemctl enable fpganet.service
systemctl restart fpganet.service

systemctl disable isc-dhcp-server6.service
systemctl enable isc-dhcp-server.service
systemctl restart isc-dhcp-server.service
