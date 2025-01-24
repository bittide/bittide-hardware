# SPDX-FileCopyrightText: 2023 Google LLC

# SPDX-License-Identifier: CC0-1.0
{ pkgs ? import ./nixpkgs.nix {} }:

pkgs.stdenv.mkDerivation rec {
  name = "openocd-riscv";

  buildInputs = [
    pkgs.autoconf
    pkgs.automake
    pkgs.coreutils
    pkgs.git
    pkgs.libtool
    pkgs.libusb1
    pkgs.libyaml
    pkgs.pkg-config
    pkgs.texinfo
    pkgs.which
  ];

  src = pkgs.fetchgit {
    url = "https://github.com/riscv-collab/riscv-openocd";
    rev = "ea8f9d51954b979ff6b4d90afa70352763199b63";
    sha256 = "sha256-cc4OebtCCsZyaMIlTfpUe26MwZ8WnrFt+IYQ+B2Hzww=";
    fetchSubmodules = true;
    deepClone = true;
    postFetch = ''
      # See: https://github.com/NixOS/nixpkgs/issues/8567#issuecomment-1846499599
      find "$out/" -type d -name '.git' -exec rm -rf {} ';'
    '';
  };

  installPhase = ''
    SKIP_SUBMODULE=1 ./bootstrap
    ./configure --enable-ftdi --enable-dummy --prefix=$out
    make -j $(nproc)
    make install
    mv $out/bin/openocd $out/bin/openocd-riscv
  '';
}
