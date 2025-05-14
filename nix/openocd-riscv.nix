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
    pkgs.jimtcl
  ];

  src = pkgs.fetchgit {
    url = "https://github.com/riscv-collab/riscv-openocd";
    rev = "1aebdf8e3025e8a2ac65a1ebcdccd11448d9b46e";
    sha256 = "sha256-GHRo8oeCJaG8DrmiwuwpHWGF9AEWEgqtoHup3O9NeUg=";
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
    mv $out/bin/openocd $out/bin/openocd-riscv
  '';
}
