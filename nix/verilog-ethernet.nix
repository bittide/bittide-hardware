{ pkgs ? import ./nixpkgs.nix {} }:

pkgs.stdenv.mkDerivation rec {
  name = "verilog-ethernet";
  src = verilog-ethernet-patched;

  verilog-ethernet-src = pkgs.fetchgit {
    url = "https://github.com/alexforencich/verilog-ethernet.git";
    rev = "baac5f8d811d43853d59d69957975ead8bbed088";
    sha256 = "sha256-rxoUHjOxxQc/JjEp06vibCJ2OIWbsbEtnkqS1gS+A7g=";
  };

  verilog-ethernet-patched = pkgs.applyPatches {
    name = "verilog-ethernet-patched";
    src = verilog-ethernet-src;
    patches = [
      (pkgs.fetchpatch {
        url = "https://github.com/lmbollen/verilog-ethernet/commit/f762edd71dfebc129dacac64ff5cd5fbf7d67801.patch";
        hash = "sha256-hKHsNViPInDKzOP3OwMZFlQ0RjYYTFPRu6rOJF1g0hQ=";
      })
    ];
  };

  installPhase = ''
    mkdir -p $out
    mv * $out
  '';
}
