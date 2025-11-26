# SPDX-FileCopyrightText: 2025 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    mdbook-drawio-flake = {
      url = "github:QBayLogic/mdbook-drawio";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay, pre-commit-hooks, mdbook-drawio-flake }: flake-utils.lib.eachDefaultSystem (
    system:
      let
        overlays = [ (import rust-overlay) ];
        verilog-ethernet = import ./nix/verilog-ethernet.nix {
          inherit pkgs;
        };
        openocd-riscv = import ./nix/openocd-riscv.nix {
          inherit pkgs;
        };
        mc = import ./nix/mc.nix {
          inherit pkgs;
        };
        pkgs = import nixpkgs {
          inherit system overlays;
          config = {
            allowUnfree = true;
          };
        };
        git-hooks-config = import ./nix/git-hooks.nix { inherit pkgs; };
        # Import your local git hooks configuration
        preCommitHook = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = git-hooks-config.hooks;
        };
        mdbook-drawio = mdbook-drawio-flake.defaultPackage.${system};
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.cabal-install
            pkgs.haskellPackages.cabal-gild
            pkgs.haskellPackages.fourmolu
            pkgs.dtc
            pkgs.gcc
            pkgs.llvmPackages.clang-unwrapped

            pkgs.haskell-language-server
            pkgs.ghc
            pkgs.pkg-config
            pkgs.python3
            pkgs.python3Packages.matplotlib
            pkgs.python3Packages.scipy
            pkgs.python3Packages.gitpython
            pkgs.python3Packages.pyaml
            pkgs.libz
            pkgs.sbt
            pkgs.scala
            pkgs.jre8
            pkgs.verilator
            pkgs.which
            pkgs.jq
            pkgs.unzip
            pkgs.flock

            # https://discourse.nixos.org/t/non-interactive-bash-errors-from-flake-nix-mkshell/33310
            pkgs.bashInteractive

            # Simulation report generation
            pkgs.dot2tex
            pkgs.texlive.combined.scheme-medium
            pkgs.poppler-utils

            (pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml)

            # For Cabal to clone git repos
            pkgs.git
            pkgs.cacert

            # HDL dependencies
            verilog-ethernet

            # CI scripts
            pkgs.python3Packages.docopt
            pkgs.python3Packages.python-dateutil
            mc
            pkgs.pcre
            pkgs.getent
            pkgs.gh

            # VexRiscv OpenOCD
            openocd-riscv
            pkgs.gdb

            # UART communication
            pkgs.picocom

            # mdbook dependencies
            pkgs.mdbook
            pkgs.drawio-headless
            mdbook-drawio
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
            ${preCommitHook.shellHook}
          '';
        };
      }
  );
}
