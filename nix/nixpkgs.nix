{ sources ? import ./sources.nix }:

let
  moz_overlay = import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/d38863db88e100866b3e494a651ee4962b762fcc.tar.gz);
  overlay = _: nixpkgs: {
    # Nix tooling
    niv = (import sources.niv {}).niv;
    gitignore = import sources.gitignore { inherit (nixpkgs) lib; };

    # Haskell overrides
    haskellPackages = nixpkgs.haskellPackages.override {
      overrides = self: super: {
        # Disables libray profiling, documentation building, and building/running
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

in import sources.nixpkgs { overlays = [ moz_overlay overlay ]; }
