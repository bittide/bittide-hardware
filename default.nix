{ nixpkgs ? import ./nix/nixpkgs.nix {} }:

with nixpkgs.pkgs.haskellPackages;

{ inherit bittide-extra bittide; }
