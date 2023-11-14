{ pkgs ? import ./nixpkgs.nix {} }:

pkgs.stdenv.mkDerivation rec {
  name = "mc";

  src = pkgs.fetchurl {
    url = "https://dl.min.io/client/mc/release/linux-amd64/archive/mc.RELEASE.2023-10-24T05-18-28Z";
    hash = "sha256-XxKSa2RrUzzeoaVIxURgpNrXjye4sX05m6Av9O42jk0=";
  };

  unpackPhase = ":";

  installPhase = ''
    mkdir -p $out/bin
    cp $src $out/bin/mc
    chmod +x $out/bin/mc
  '';
}
