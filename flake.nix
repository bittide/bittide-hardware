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
    hls-source = {
      url = "github:haskell/haskell-language-server?ref=2.13.0.0";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay, pre-commit-hooks, mdbook-drawio-flake, hls-source }: flake-utils.lib.eachDefaultSystem (
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

        # What GHC version HLS should support
        hlsGhcVersion = "910";
        # What the HLS version should be
        # NOTE: this does not *SET* the version! It merely 'pretends' to be the version set by this value
        # To update the actual HLS version, run: `nix flake update hls-source`
        # If weird HLS dependency issues accur, this is the variable you should most likely update
        hlsVersion = "2.13.0.0";

        hls-overlay = final: prev: let
          setSource = pkg: src-path: (prev.haskell.lib.compose.overrideCabal (drv: {
            version = hlsVersion;
            src = hls-source // {
              outPath = "${hls-source.outPath}/${src-path}";
            };
            # Disable HLS tests for faster compilation :)
            doCheck = false;
          }) pkg);

          setSourceDirect = pkg: prev.haskell.lib.compose.overrideCabal (drv: {
            version = hlsVersion;
            src = hls-source;
          }) pkg;

          addEditDistance = pkg: pkg.overrideAttrs (drv: {
            # edit-distance doesn't get automatically added to the dependencies list for some reason
            # This manually adds it to the dependencies
            propagatedBuildInputs = drv.propagatedBuildInputs ++ [ prev.haskell.packages."ghc${hlsGhcVersion}".edit-distance ];
          });

          # Make the new package set containing the up-to-date versions of HLS and it's in-repository plugins
          latestHLSPackages = prev.haskell.packages."ghc${hlsGhcVersion}".extend (_: p: {
            ghcide = setSource p.ghcide "ghcide";
            hls-graph = addEditDistance (setSource p.hls-graph "hls-graph");
            hls-test-utils = setSource p.hls-test-utils "hls-test-utils";
            hls-plugin-api = setSource p.hls-plugin-api "hls-plugin-api";
            haskell-language-server = setSourceDirect p.haskell-language-server;
          });
        in {
          # Override haskell-* sets to match the one containing the latest HLS packages
          haskell = prev.haskell // { "ghc${hlsGhcVersion}" = latestHLSPackages; };
          haskellPackages = latestHLSPackages;
          haskell-language-server = latestHLSPackages.haskell-language-server;
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
            # python3Full is removed
            # Bluetooth is enabled by default and tkinter has been split off into the package set
            pkgs.python3
            pkgs.python3Packages.tkinter
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
