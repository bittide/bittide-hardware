# See https://github.com/cachix/git-hooks.nix/tree/master
{ pkgs, ... }:

{
  # This is the configuration passed to the pre-commit-hooks module.
  hooks = {

    reuse.enable = true; # check for REUSE compliance
    trim-trailing-whitespace.enable = true;
    end-of-file-fixer.enable = true;
    check-added-large-files.enable = true;

    # Enable a formatter for Haskell
    fourmolu.enable = true;
    cabal-gild.enable = true;

    # Excludes, I dont know a different way to do this
    reuse.excludes = ["^clash-vexriscv/.*"];
    trim-trailing-whitespace.excludes = ["^clash-vexriscv/.*"];
    end-of-file-fixer.excludes = ["^clash-vexriscv/.*"];
    check-added-large-files.excludes = ["^clash-vexriscv/.*"];
    fourmolu.excludes = ["^clash-vexriscv/.*"];
    cabal-gild.excludes = ["^clash-vexriscv/.*"];

    # Rust
    # We can not use rust hooks yet because they assume the repository root contains a `Cargo.toml` file.
    # TODO: Create workaround for this issue.
    # cargo-check.enable = true;
    # clippy.enable = true;
    # rustfmt.enable = true;
    update-ci-yml = {
      enable = true;
      name = "Create ci.yml from ci.src";
      entry = ".github/scripts/update-ci-yml.sh";
      # 2. Make the `files` pattern more specific (good practice).
      files = "\\.github/workflows/ci\\.src";
      stages = [ "pre-commit" ];
      pass_filenames = false;
      verbose = true;
    };
  };
}
