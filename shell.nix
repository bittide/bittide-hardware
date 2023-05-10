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
      pkgs.buildPackages.cabal-install
      pkgs.buildPackages.dtc
      pkgs.buildPackages.gcc
      pkgs.buildPackages.ghc
      pkgs.buildPackages.pkg-config
      pkgs.buildPackages.python310Full
      pkgs.buildPackages.python310Packages.matplotlib
      pkgs.buildPackages.python310Packages.scipy
      pkgs.buildPackages.sbt
      pkgs.buildPackages.scala
      pkgs.buildPackages.verilator
      pkgs.buildPackages.which
      pkgs.buildPackages.jq

      # Simulation report generation
      pkgs.buildPackages.dot2tex
      pkgs.buildPackages.texlive.combined.scheme-medium
      pkgs.buildPackages.poppler_utils

      (pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml)

      # RiscV formal tests
      pkgs.buildPackages.boolector
      pkgs.buildPackages.symbiyosys
      pkgs.buildPackages.yosys
      pkgs.buildPackages.z3

      # For Cabal to clone git repos
      pkgs.buildPackages.git

      # HDL dependencies
      pkgs.verilog-ethernet
    ]
    ;

  shellHook = ''
    # Prevents Perl warnings
    export LC_ALL="C.UTF-8";
    export VERILOG_ETHERNET_SRC="${pkgs.verilog-ethernet}"

    # Mixing Nix Cabal and non-Nix Cabal yields some weird linking errors.
    export CABAL_DIR="$HOME/.cabal-nix";
  '';
}
