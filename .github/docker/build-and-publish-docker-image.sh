#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2024 Google LLC

# SPDX-License-Identifier: CC0-1.0
set -xeo pipefail

REPO="ghcr.io/clash-lang"
NAME="clash-vexriscv-ci"
DIR=$(dirname "$0")
now=$(date +%Y%m%d)

if [[ "$1" == "-y" ]]; then
  push=y
elif [[ "$1" != "" ]]; then
  echo "Unrecognized argument: $1" >&2
  exit 1
fi

UBUNTU_VERSION=jammy-20240125
GHC_VERSIONS=(  "9.4.8"  "9.2.8"   "9.0.2")
CABAL_VERSIONS=("3.10.2.0" "3.10.2.0" "3.10.2.0")

for i in "${!GHC_VERSIONS[@]}"
do
  GHC_VERSION="${GHC_VERSIONS[i]}"
  CABAL_VERSION="${CABAL_VERSIONS[i]}"

  docker buildx build \
    --build-arg UBUNTU_VERSION=${UBUNTU_VERSION} \
    --build-arg cabal_version=${CABAL_VERSION} \
    --build-arg ghc_version=${GHC_VERSION} \
    -t "${REPO}/${NAME}:${GHC_VERSION}-$now" \
    "$DIR"
done

if [[ "${push}" == "" ]]; then
  read -p "Push to GitHub? (y/N) " push
fi

if [[ $push =~ ^[Yy]$ ]]; then
  for i in "${!GHC_VERSIONS[@]}"
  do
    GHC_VERSION="${GHC_VERSIONS[i]}"
    docker push "${REPO}/${NAME}:${GHC_VERSION}-$now"
  done
else
  echo "Skipping push to container registry"
fi
