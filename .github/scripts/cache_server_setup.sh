#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
set -euf -o pipefail

MINIO_VERSION="20230823100706.0.0"
MINIO_HASH="b591fce770d2aaed7e9fcb62308efa781fe7c411481c442041df12ca2e157eea"

MINIO_ROOT="root"
MINIO_PASSWORD=$((tr -dc A-Za-z0-9 </dev/urandom | head -c 20) || true)

echo "MINIO_ROOT_USER=root" > minio
echo "MINIO_ROOT_PASSWORD=${MINIO_PASSWORD}" >> minio
echo "MINIO_VOLUMES=/var/cache/minio" >> minio
echo "MINIO_CONSOLE_ADDRESS=":9090"" >> minio
echo "MINIO_SERVER_URL=https://hetzner0.bittide.clash-lang.org" >> minio
echo "MINIO_BROWSER_REDIRECT_URL=https://hetzner0.bittide.clash-lang.org/minio/ui" >> minio
sudo cp minio /etc/default/minio

# Stop previous minio instance
sudo systemctl stop minio.service || true

# Download and install minio
sudo apt install curl -y
curl -o minio.deb -L "https://dl.min.io/server/minio/release/linux-amd64/archive/minio_${MINIO_VERSION}_amd64.deb"
echo "${MINIO_HASH}  minio.deb" | shasum -a 256 -c
sudo apt install ./minio.deb
rm minio.deb

# systemd starts minio as 'minio-user', but Debian package does not create this
# user on its own.
sudo adduser --disabled-password --gecos "" minio-user

# Start service
sudo systemctl start minio.service
