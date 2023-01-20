#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
set -euo pipefail
IFS=$'\n\t'

HERE=$(dirname $0)
ROOT="$(git rev-parse --show-toplevel)"
REPO="ghcr.io/clash-lang"
NAME="nixos-bittide-hardware"
TODAY="$(date +%F)"

cd "${HERE}"

cp "${ROOT}/default.nix" .
cp "${ROOT}/shell.nix" .
cp -ap "${ROOT}/nix" .

docker build -t "${REPO}/${NAME}:$TODAY" -t "${REPO}/${NAME}:latest" .

read -p "Push to GitHub? (y/N) " push

if [[ $push =~ ^[Yy]$ ]]; then
        docker push "${REPO}/${NAME}:$TODAY"
        docker push "${REPO}/${NAME}:latest"
else
        echo "Skipping push to container registry"
fi
