{ pkgs }:

pkgs.stdenv.mkDerivation rec {
  name = "openocd-vexriscv";

  buildInputs = [
    pkgs.autoconf
    pkgs.automake
    pkgs.coreutils
    pkgs.git
    pkgs.libtool
    pkgs.libusb1
    pkgs.libyaml
    pkgs.pkg-config
    pkgs.texinfo
    pkgs.which
  ];

  src = pkgs.fetchFromGitHub {
    owner = "martijnbastiaan";
    repo = "riscv-openocd";
    # branch = "no-submodules-058dfa50d625893bee9fecf8d604141911fac125"
    rev = "8af10d81e82ea59e23717ba928daeeaac2404a9e";
    sha256 = "sha256-fFcajCI5Xk+AXmR8s5yTFBnj1AmlRzNU/C5/L4z5QMQ=";
  };

  installPhase = ''
    runHook preInstall
    SKIP_SUBMODULE=1 ./bootstrap
    ./configure --enable-ftdi --enable-dummy --prefix=$out
    make CFLAGS="-Wno-error=calloc-transposed-args" -j $(nproc)
    make install
    mv $out/bin/openocd $out/bin/openocd-vexriscv
    runHook postInstall
  '';
}
