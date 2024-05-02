{ pkgs ? import ./nixpkgs.nix {} }:

pkgs.stdenv.mkDerivation rec {
  name = "verilog-ethernet";

  src = pkgs.fetchgit {
    url = "https://github.com/alexforencich/verilog-ethernet.git";
    rev = "baac5f8d811d43853d59d69957975ead8bbed088";
    sha256 = "sha256-rxoUHjOxxQc/JjEp06vibCJ2OIWbsbEtnkqS1gS+A7g=";
  };

  installPhase = ''
    mkdir -p $out
    mv * $out
  '';
}
