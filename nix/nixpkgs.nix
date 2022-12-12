{ sources ? import ./sources.nix }:

let
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
        clash-protocols =
          self.applyPrefs
	    (self.callCabal2nix "clash-protocols" sources.clash-protocols {});

        circuit-notation =
	  self.applyPrefs
            (self.callCabal2nix "circuit-notation" sources.circuit-notation {});

        tasty-hedgehog =
	  self.applyPrefs
            (self.callCabal2nix "tasty-hedgehog" sources.tasty-hedgehog {});

        clash-prelude =
	  self.applyPrefs
	    (nixpkgs.haskell.lib.disableCabalFlag
              (self.callCabal2nix "clash-prelude" (sources.clash-compiler + "/clash-prelude") {})
              "multiple-hidden");

        clash-prelude-hedgehog =
	  self.applyPrefs
            (self.callCabal2nix "clash-prelude-hedgehog" (sources.clash-compiler + "/clash-prelude-hedgehog") {});

        clash-lib =
	  self.applyPrefs
            (self.callCabal2nix "clash-lib" (sources.clash-compiler + "/clash-lib") {});

        clash-ghc =
	  self.applyPrefs
            (self.callCabal2nix "clash-ghc" (sources.clash-compiler + "/clash-ghc") {});

        clash-cores =
	  self.applyPrefs
            (self.callCabal2nix "clash-cores" (sources.clash-compiler + "/clash-cores") {});

        # Internal overrides
        bittide-extra = self.applyPrefs (import ../bittide-extra { inherit nixpkgs; });
        contranomy    = self.applyPrefs (import ../contranomy    { inherit nixpkgs; });
        bittide       = self.applyPrefs (import ../bittide       { inherit nixpkgs; });
      };
    };
  };

in import sources.nixpkgs { overlays = [ overlay ]; }

