#!/bin/bash
# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

FORCE="$1"
set -xuo pipefail
HERE=$(realpath "$(dirname "$0")")
source "$HERE/dhcp_config" || exit $?

echo "Checking whether ${INTERFACE} exist"
ip addr show "$INTERFACE" > /dev/null || exit $?
echo "${INTERFACE} exists"

FILES="/etc/dhcp/dhcpd.conf /etc/default/isc-dhcp-server"
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

cp "$HERE/dhcpd.conf" /etc/dhcp/
echo "INTERFACESv4=\"$INTERFACE\"" > /etc/default/isc-dhcp-server

systemctl daemon-reload

systemctl disable isc-dhcp-server6.service
systemctl enable isc-dhcp-server.service
systemctl restart isc-dhcp-server.service
