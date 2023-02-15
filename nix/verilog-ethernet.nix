{ pkgs ? import ./nixpkgs.nix {} }:

pkgs.stdenv.mkDerivation rec {
  name = "verilog-ethernet";

  src = pkgs.fetchgit {
    url = "https://github.com/alexforencich/verilog-ethernet.git";
    rev = "ab0c382123f2f0b80480d99c3d9837af028e7295";
    sha256 = "sha256-xFhQXSRb+aaL5rHRmwgKCpJmgjpWib+PvaYp5kgP6To=";
  };

  installPhase = ''
    mkdir -p $out
    mv * $out
  '';
}
