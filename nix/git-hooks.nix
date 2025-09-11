# See https://github.com/cachix/git-hooks.nix/tree/master
{ pkgs, ... }:
{
  # This is the configuration passed to the pre-commit-hooks module.
  hooks = pkgs.lib.mapAttrs (
    name: hookConfig:
    {
      # This sets the default `excludes` list for every hook.
      # The `//` operator merges the two attribute sets.
      # If a hook already has an `excludes` list, it will be kept.
      excludes = [ "^clash-vexriscv/.*" ];
    } // hookConfig)
    {

    reuse.enable = true; # check for REUSE compliance
    trim-trailing-whitespace.enable = true;
    end-of-file-fixer.enable = true;
    check-added-large-files.enable = true;
    check-yaml.enable = true;
    check-toml.enable = true;
    check-json.enable = true;

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
    };

    my-clippy = {
      enable = true;
      name = "Custom clippy hook";
      entry = "./cargo.sh clippy --all-features -- -Dwarnings";
      language = "system";
      files = "\\.(rs)$";
      pass_filenames = false;
    };

    my-rustfmt = {
      enable = true;
      name = "Custom rustfmt hook";
      entry = "./cargo.sh fmt --all";
      language = "system";
      files = "\\.(rs)$";
      pass_filenames = false;
    };

    # TODO: https://github.com/bittide/bittide-hardware/issues/972
    # We can not use rust hooks yet because they assume the repository root contains a `Cargo.toml` file.
    # rustfmt.enable = true;
    # cargo-check.enable = true;
    # clippy.enable = true;
  };
}
