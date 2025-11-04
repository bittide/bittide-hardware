# SPDX-FileCopyrightText: 2025 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
{
  description = "Clash VexRiscv development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

    flake-utils.url = "github:numtide/flake-utils";

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            rust-overlay.overlays.default
            (self: super: {
              # Nix tooling
              openocd-riscv = self.callPackage ./nix/openocd-riscv.nix { };
            })
          ];
        };

        rustToolchain = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
      in
      {
        devShells.default = pkgs.mkShell {
          name = "clash-vexriscv-shell";

          buildInputs = [
            pkgs.gcc
            pkgs.pkg-config
            pkgs.sbt
            pkgs.scala
            pkgs.verilator

            # Haskell toolchain
            pkgs.cabal-install
            pkgs.haskellPackages.fourmolu
            pkgs.ghc

            rustToolchain

            pkgs.openocd-riscv
            pkgs.gdb

            # For Cabal to clone git repos
            pkgs.git

            # C Tools
            pkgs.clang
          ];

          shellHook = ''
            # Prevents Perl warnings
            export LC_ALL="C.UTF-8";

            # Add repo utilities to path
            export PATH="$(git rev-parse --show-toplevel)/nix/bin:$PATH";
          '';
        };

      }
    );
}
