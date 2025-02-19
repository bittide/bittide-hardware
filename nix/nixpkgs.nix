{ sources ? import ./sources.nix }:

let
  rust_overlay = import (builtins.fetchTarball "https://github.com/oxalica/rust-overlay/archive/8509a51241c407d583b1963d5079585a992506e8.tar.gz");
  overlay = _: nixpkgs: {
    # Nix tooling
    gitignore = import sources.gitignore { inherit (nixpkgs) lib; };

    verilog-ethernet = import ./verilog-ethernet.nix { inherit (nixpkgs) pkgs; };
    mc = import ./mc.nix { inherit (nixpkgs) pkgs; };
    openocd-vexriscv = import ./openocd-vexriscv.nix { inherit (nixpkgs) pkgs; };
    openocd-riscv = import ./openocd-riscv.nix { inherit (nixpkgs) pkgs; };

    # Haskell overrides
    haskellPackages = nixpkgs.haskellPackages.override {
      overrides = self: super: {
        # Disables library profiling, documentation building, and building/running
        # test suites to speed up compilation times. This should only be applied
        # to local packages. Adding them to others will defeat caching.
        applyPrefs = p:
          (nixpkgs.haskell.lib.disableLibraryProfiling
            (nixpkgs.haskell.lib.dontHaddock
              (nixpkgs.haskell.lib.dontCheck p)));

        # External overrides
        # .. no external overrides yet ..

        # Internal overrides
        # .. no internal overrides yet ..
      };
    };
  };

in import sources.nixpkgs { overlays = [ rust_overlay overlay ]; }
