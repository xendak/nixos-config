{ stdenv, zig }:

stdenv.mkDerivation {
  pname = "template";
  version = "0.1.0";

  src = ./.;

  nativeBuildInputs = [ zig ];

  buildPhase = ''
    zig build -Drelease-safe=true
  '';

  installPhase = ''
    zig build install --prefix $out
  '';
}
