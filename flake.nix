# SPDX-FileCopyrightText: 2025 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
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
      url = "github:rowanG077/mdbook-drawio?ref=rowan/mdbook-5";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay, pre-commit-hooks, mdbook-drawio-flake }: flake-utils.lib.eachDefaultSystem (
    system:
      let
        verilog-ethernet = import ./nix/verilog-ethernet.nix {
          inherit pkgs;
        };
        openocd-riscv = import ./nix/openocd-riscv.nix {
          inherit pkgs;
        };
        mc = import ./nix/mc.nix {
          inherit pkgs;
        };

        hls-overlay = final: prev: {
          haskell-language-server =
            (prev.haskell-language-server.override { supportedGhcVersions = [ "910" ]; })
            .overrideDerivation (old: {
              src = pkgs.fetchFromGitHub {
                owner = "haskell";
                repo = "haskell-language-server";
                rev = "88ccebe0649f7c41be97d49a986bbfd4185982f6";
                sha256 = "sha256-hR4MtfespgqAEa/vWXNsIOcEcLQNIVaEAHqZJbTaV/g=";
              };
            });
        };
        overlays = [ (import rust-overlay) hls-overlay ];

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
            pkgs.haskell.compiler.ghc910
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

            # CI dependencies
            pkgs.fd
            pkgs.sd
            pkgs.ripgrep
            pkgs.tomlq
            (pkgs.rustPlatform.buildRustPackage (finalAttrs: {
              pname = "elf-limits";
              version = "0.1.0";
              src = pkgs.fetchFromGitHub {
                owner = "cuddlefishie";
                repo = "elf-limits";
                rev = "5b7bd41b0167ea6e506b5f856f0252362a54721a";
                sha256 = "sha256-DMc/B8VajjQgGNiftq99tfn7IRWDenlaCTMGddbDYLY=";
              };
              cargoHash = "sha256-0o/2nSKeF+AbnI9JmDdKiLpoXi2WimvDhcAkDknVK8M=";
              doCheck = false;
            }))
          ];

          shellHook = ''
            # Prevents Perl warnings
            export LC_ALL="C.UTF-8";
            export VERILOG_ETHERNET_SRC="${verilog-ethernet}"
            export OPENOCD_DIST="${openocd-riscv}"

            # We use unstable features (floating point), we don't want to hear about it
            # every time we build.
            export RUSTFLAGS="-Aunstable-features"

            # Allow writing 'shake ...' instead of 'cabal run shake -- ...'
            export PATH="$(git rev-parse --show-toplevel)/nix/bin:$PATH";
            ${preCommitHook.shellHook}
          '';
        };
      }
  );
}
