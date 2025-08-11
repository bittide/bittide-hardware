# See https://github.com/cachix/git-hooks.nix/tree/master
{ pkgs, ... }:
let
  excludes = ["^clash-vexriscv/.*"];
in
{
  # This is the configuration passed to the pre-commit-hooks module.
  hooks = {

    reuse.enable = true; # check for REUSE compliance
    trim-trailing-whitespace.enable = true;
    end-of-file-fixer.enable = true;
    check-added-large-files.enable = true;

    # Enable a formatter for Haskell
    cabal-gild.enable = true;


    # Fourmolu and header fix, Fourmolu sometimes messes up the header comments, so
    # we need to fix them manually using the fix_spdx_header.py script.
    format-hs = {
      enable = true;
      name = "Format Haskell files and fix SPDX headers";
      entry = ".github/scripts/fourmolu.sh";
      language = "system";
      files = "\\.(hs|hs-boot)$";
      excludes = excludes;
    };

    # Excludes, I dont know a different way to do this
    reuse.excludes = excludes;
    trim-trailing-whitespace.excludes = excludes;
    end-of-file-fixer.excludes = excludes;
    check-added-large-files.excludes = excludes;
    cabal-gild.excludes = excludes;


    # TODO: https://github.com/bittide/bittide-hardware/issues/972
    # We can not use rust hooks yet because they assume the repository root contains a `Cargo.toml` file.
    # rustfmt.enable = true;
    # cargo-check.enable = true;
    # clippy.enable = true;
  };
}
