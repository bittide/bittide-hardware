# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

{ pkgs ? import nix/nixpkgs.nix {} }:
pkgs.mkShell {
  name = "shell";
  buildInputs =
    [
      pkgs.buildPackages.cabal-install
      pkgs.buildPackages.gcc
      pkgs.buildPackages.ghc
      pkgs.buildPackages.pkg-config
      pkgs.buildPackages.sbt
      pkgs.buildPackages.scala
      pkgs.buildPackages.verilator

      (pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml)

      # For Cabal to clone git repos
      pkgs.buildPackages.git
    ]
    ;

  shellHook = ''
    # Prevents Perl warnings
    export LC_ALL="C.UTF-8";

    # Mixing Nix Cabal and non-Nix Cabal yields some weird linking errors.
    export CABAL_DIR="$HOME/.cabal-nix";
  '';
}
