# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
FROM ubuntu:22.04

RUN \
  apt-get update && \
  apt-get install -y curl xz-utils sudo git && \
  rm -rf /var/lib/apt/lists/*

RUN \
  groupadd -r nixbld && \
  for n in $(seq 1 10); do \
    useradd \
      -c "Nix build user $n"\
      -d /var/empty \
      -g nixbld \
      -G nixbld \
      -M -N -r \
      -s "$(command -v nologin)" "nixbld$n";\
  done && \
  curl -L https://nixos.org/nix/install --output install_nix.sh && \
  sh install_nix.sh

ENV \
  NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt \
  MANPATH="/root/.nix-profile/share/man" \
  PATH="/root/.nix-profile/bin:$PATH" \
  LC_ALL=en_US.UTF-8

WORKDIR /build
COPY rust-toolchain.toml rust-toolchain.toml
COPY flake.nix flake.nix
COPY flake.lock flake.lock
COPY nix /build/nix

RUN \
  nix profile --extra-experimental-features "nix-command flakes" install nixpkgs#cachix

RUN \
  USER=root cachix use bittide-hardware

RUN \
  nix develop -j$(nproc) --extra-experimental-features "nix-command flakes" --command bash -c "echo OK"

# Vivado hacks / dependencies:
RUN \
  apt-get update && \
  apt-get install language-pack-en-base -y && \
  locale-gen en_US.UTF-8 && \
  update-locale && \
  apt-get install libtinfo5 libncurses5 libx11-6 -y

# For HITL purposes we need to run as a normal user. 1001 is the user with plugdev
# access on the runner.
RUN \
  addgroup --gid 1001 bittide && \
  adduser --uid 1001 bittide --ingroup bittide

COPY git-nix-shell /usr/bin/git-nix-shell
