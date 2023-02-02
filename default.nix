# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
{ nixpkgs ? import ./nix/nixpkgs.nix {} }:

with nixpkgs.pkgs.haskellPackages;

{ inherit bittide-extra bittide; }
