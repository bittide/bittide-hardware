# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

{ pkgs ? import nix/nixpkgs.nix {} }:
pkgs.mkShell {
  name = "shell";
  buildInputs =
    # Ideally, we'd add all (Hackage) dependencies to the dependency list so we
    # don't have to compile everything ourselves. I haven't figured out how to
    # do this yet without introducing conflicts with `cabal.project`.
    #
    # pkgs.haskellPackages.bittide-extra.env.nativeBuildInputs ++
    #
    [
      pkgs.cabal-install
      pkgs.haskellPackages.cabal-gild
      pkgs.dtc
      pkgs.gcc

      # Clash throws an error on 9.4.8 to 9.6.2
      # See https://github.com/clash-lang/clash-compiler/pull/2711/files#diff-b335630551682c19a781afebcf4d07bf978fb1f8ac04c6bf87428ed5106870f5R80
      pkgs.haskell.compiler.ghc947
      pkgs.pkg-config
      pkgs.python311Full
      pkgs.python311Packages.matplotlib
      pkgs.python311Packages.scipy
      pkgs.python311Packages.GitPython
      pkgs.python311Packages.pyaml
      pkgs.sbt
      pkgs.scala
      pkgs.verilator
      pkgs.which
      pkgs.jq
      pkgs.unzip
      pkgs.ndisc6

      # Simulation report generation
      pkgs.dot2tex
      pkgs.texlive.combined.scheme-medium
      pkgs.poppler_utils

      (pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml)

      # For Cabal to clone git repos
      pkgs.git
      pkgs.cacert

      # HDL dependencies
      pkgs.verilog-ethernet

      # CI cache scripts
      pkgs.python311Packages.docopt
      pkgs.python311Packages.dateutil
      pkgs.mc

      # VexRiscv OpenOCD
      pkgs.openocd-vexriscv
      pkgs.gdb

      # UART communication
      pkgs.picocom

      # For upgrading Nix env. To update dependencies (within bounds of the currently
      # tracking NixOS version) use:
      #
      #   niv update
      #
      # If you want to upgrade nixpkgs to another NixOS version, use:
      #
      #   niv update nixpkgs -b nixos-23.11
      #
      pkgs.niv
    ]
    ;

  shellHook = ''
    # Prevents Perl warnings
    export LC_ALL="C.UTF-8";
    export VERILOG_ETHERNET_SRC="${pkgs.verilog-ethernet}"
    export OPENOCD_DIST="${pkgs.openocd-vexriscv}"

    # Mixing Nix Cabal and non-Nix Cabal yields some weird linking errors.
    export CABAL_DIR="$HOME/.cabal-nix";

    # Allow writing 'shake ...' instead of 'cabal run shake -- ...'
    export PATH="$(git rev-parse --show-toplevel)/nix/bin:$PATH";
  '';
}
