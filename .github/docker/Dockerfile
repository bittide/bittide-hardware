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
  MANPATH="/root/.nix-profile/share/man:$MANPATH" \
  PATH="/root/.nix-profile/bin:$PATH"

WORKDIR /build
COPY rust-toolchain.toml rust-toolchain.toml
COPY default.nix default.nix
COPY shell.nix shell.nix
COPY nix /build/nix

RUN \
  nix-shell -j32 --run "echo OK"

RUN \
  echo '#!/usr/bin/env bash'                                                >> /usr/bin/git-nix-shell && \
  echo 'nix-shell "${GITHUB_WORKSPACE}/shell.nix" "${@:2}" --run "bash $1"' >> /usr/bin/git-nix-shell && \
  chmod +x /usr/bin/git-nix-shell
