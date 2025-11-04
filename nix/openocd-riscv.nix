# SPDX-FileCopyrightText: 2023 Google LLC

# SPDX-License-Identifier: CC0-1.0
{ pkgs }:

pkgs.stdenv.mkDerivation rec {
  name = "openocd-riscv";

  buildInputs = [
    pkgs.autoconf
    pkgs.automake
    pkgs.coreutils
    pkgs.git
    pkgs.jimtcl
    pkgs.libtool
    pkgs.libusb1
    pkgs.libyaml
    pkgs.pkg-config
    pkgs.texinfo
    pkgs.which
  ];

  src = pkgs.fetchgit {
    # Upstream repo (riscv-collab/riscv-openocd) contains submodules, which Nix can't
    # reliably hash. My fork removes the submodules (and provides a script to build new
    # submodule free branches).
    url = "https://github.com/martijnbastiaan/riscv-openocd";
    rev = "refs/heads/no-submodules-ea8f9d51954b979ff6b4d90afa70352763199b63";
    sha256 = "sha256-9UmjCCvAWIUoczqqTtgEmoHSTcKLSbG3nLrPgwz1m9o=";
  };

  installPhase = ''
    SKIP_SUBMODULE=1 ./bootstrap
    ./configure --enable-ftdi --enable-dummy --prefix=$out
    make -j $(nproc)
    make install
    mv $out/bin/openocd $out/bin/openocd-riscv
  '';
}
