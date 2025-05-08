# SPDX-FileCopyrightText: 2025 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay }: flake-utils.lib.eachDefaultSystem (
    system:
      let
        overlays = [ (import rust-overlay) ];
        verilog-ethernet = import ./nix/verilog-ethernet.nix {
          inherit pkgs;
        };
        openocd-riscv = import ./clash-vexriscv/nix/openocd-riscv.nix {
          inherit pkgs;
        };
        mc = import ./nix/mc.nix {
          inherit pkgs;
        };
        pkgs = import nixpkgs {
          inherit system overlays;
        };
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.cabal-install
            pkgs.haskellPackages.cabal-gild
            pkgs.haskellPackages.fourmolu
            pkgs.dtc
            pkgs.gcc

            pkgs.haskell-language-server
            pkgs.ghc
            pkgs.pkg-config
            pkgs.python311Full
            pkgs.python311Packages.matplotlib
            pkgs.python311Packages.scipy
            pkgs.python311Packages.GitPython
            pkgs.python311Packages.pyaml
            pkgs.libz
            pkgs.sbt
            pkgs.scala
            pkgs.verilator
            pkgs.which
            pkgs.jq
            pkgs.unzip
            pkgs.ndisc6

            # https://discourse.nixos.org/t/non-interactive-bash-errors-from-flake-nix-mkshell/33310
            pkgs.bashInteractive

            # Simulation report generation
            pkgs.dot2tex
            pkgs.texlive.combined.scheme-medium
            pkgs.poppler_utils

            (pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml)

            # For Cabal to clone git repos
            pkgs.git
            pkgs.cacert

            # HDL dependencies
            verilog-ethernet

            # CI scripts
            pkgs.python311Packages.docopt
            pkgs.python311Packages.dateutil
            mc
            pkgs.pcre
            pkgs.getent

            # GDB
            openocd-riscv
            pkgs.gdb

            # UART communication
            pkgs.picocom

            # For probe-rs
            pkgs.udev.dev
          ];

          shellHook = ''
            # Prevents Perl warnings
            export LC_ALL="C.UTF-8";
            export VERILOG_ETHERNET_SRC="${verilog-ethernet}"
            export OPENOCD_DIST="${openocd-riscv}"

            # We use unstable features (floating point), we don't want to hear about it
            # every time we build.
            export RUSTFLAGS="-Aunstable-features"

            # Mixing Nix Cabal and non-Nix Cabal yields some weird linking errors.
            export CABAL_DIR="$HOME/.cabal-nix";

            # Allow writing 'shake ...' instead of 'cabal run shake -- ...'
            export PATH="$(git rev-parse --show-toplevel)/nix/bin:$PATH";
          '';
        };
      }
  );
}
