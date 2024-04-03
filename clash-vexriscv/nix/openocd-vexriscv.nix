# SPDX-FileCopyrightText: 2023 Google LLC

# SPDX-License-Identifier: CC0-1.0
{ pkgs ? import ./nixpkgs.nix {} }:

pkgs.stdenv.mkDerivation rec {
  name = "openocd-vexriscv";

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
    url = "https://github.com/SpinalHDL/openocd_riscv.git";
    rev = "058dfa50d625893bee9fecf8d604141911fac125";
    sha256 = "sha256-UuX4Zfr9DiJx60nvBAv+9xCbWXExrk5KNSC5V5e4rsw=";
    fetchSubmodules = true;
    deepClone = true;
    postFetch = ''
      # See: https://github.com/NixOS/nixpkgs/issues/8567#issuecomment-1846499599
      find "$out/" -type d -name '.git' | xargs rm -rf
    '';
  };

  installPhase = ''
    SKIP_SUBMODULE=1 ./bootstrap
    ./configure --enable-ftdi --enable-dummy --prefix=$out
    make -j $(nproc)
    make install
    mv $out/bin/openocd $out/bin/openocd-vexriscv
  '';
}
