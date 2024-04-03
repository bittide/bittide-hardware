# SPDX-FileCopyrightText: 2022-2024 Google LLC
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
      pkgs.dtc
      pkgs.gcc
      pkgs.haskell.compiler.ghc90
      pkgs.pkg-config
      pkgs.python311Full
      pkgs.python311Packages.matplotlib
      pkgs.python311Packages.scipy
      pkgs.python311Packages.GitPython
      pkgs.sbt
      pkgs.scala
      pkgs.verilator
      pkgs.which
      pkgs.jq
      pkgs.unzip

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
