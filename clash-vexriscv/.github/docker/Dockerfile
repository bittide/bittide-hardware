# syntax=docker/dockerfile:1.2

# SPDX-FileCopyrightText: 2024 Google LLC

# SPDX-License-Identifier: CC0-1.0

ARG UBUNTU_VERSION
FROM ubuntu:$UBUNTU_VERSION AS builder

LABEL vendor="QBayLogic B.V." maintainer="devops@qbaylogic.com"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8 PREFIX=/opt

ARG DEPS_COMMON="build-essential ca-certificates curl git locales ca-certificates"

RUN apt-get update \
  && apt-get install -y --no-install-recommends $DEPS_COMMON \
  && locale-gen en_US.UTF-8 \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

FROM builder AS build-openocd-vexriscv

ARG DEPS_OPENOCD_VEXRISCV="autoconf automake libtool pkg-config libusb-1.0-0-dev libftdi-dev libhidapi-dev libusb-dev libyaml-dev"

RUN apt-get update \
  && apt-get install -y --no-install-recommends $DEPS_OPENOCD_VEXRISCV \
  && git clone --recursive https://github.com/SpinalHDL/openocd_riscv.git \
  && cd openocd_riscv \
  && ./bootstrap \
  && ./configure --enable-ftdi --enable-dummy --prefix=/opt \
  && make -j$(nproc) \
  && make install

FROM builder AS build-verilator

ARG DEPS_VERILATOR="perl python3 make autoconf g++ flex bison ccache libgoogle-perftools-dev numactl perl-doc libfl2 libfl-dev zlib1g zlib1g-dev help2man"
RUN  apt-get update \
  && apt-get install -y --no-install-recommends $DEPS_VERILATOR

ARG verilator_version="v5.020"
RUN git clone https://github.com/verilator/verilator verilator \
  && cd verilator \
  && git checkout $verilator_version \
  && autoconf \
  && ./configure --prefix $PREFIX \
  && make PREFIX=$PREFIX -j$(nproc) \
  && make PREFIX=$PREFIX install \
  && cd ../.. \
  && rm -Rf verilator

FROM builder AS build-ghc

ARG ghcup_version="0.1.22.0"

# Must be explicitly set
ARG ghc_version
ARG cabal_version

RUN curl "https://downloads.haskell.org/~ghcup/$ghcup_version/x86_64-linux-ghcup-$ghcup_version" --output /usr/bin/ghcup \
 && chmod +x /usr/bin/ghcup \
 && ghcup install ghc $ghc_version --set \
 && ghcup install cabal $cabal_version --set

FROM builder AS run

LABEL vendor="QBayLogic B.V." maintainer="devops@qbaylogic.com"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8 PATH="$PATH:/opt/bin:/root/.ghcup/bin"

ARG DEPS_RUNTIME="gnupg pkg-config openjdk-8-jdk gdb-multiarch picocom libtinfo5 libtinfo-dev build-essential curl libc6-dev libgmp10-dev python3 ccache libftdi1 libhidapi-hidraw0 libusb-1.0-0 libyaml-0-2"
RUN apt-get update \
  && apt-get install -y --no-install-recommends $DEPS_RUNTIME \
  && echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list \
  && echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list \
  && curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add \
  && apt-get update \
  && apt-get install -y --no-install-recommends sbt \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

COPY --from=build-verilator /opt /opt
COPY --from=build-openocd-vexriscv /opt /opt
COPY --from=build-ghc /root/.ghcup /root/.ghcup
