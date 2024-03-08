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
    sha256 = "sha256-LbT0L+VDFLlSrLkHa0P5pfmZHJI5uaMazrLXj8WFpck=";
    fetchSubmodules = true;
    deepClone = true;
  };

  installPhase = ''
    SKIP_SUBMODULE=1 ./bootstrap
    ./configure --enable-ftdi --enable-dummy --prefix=$out
    make -j $(nproc)
    make install
    mv $out/bin/openocd $out/bin/openocd-vexriscv
  '';
}
