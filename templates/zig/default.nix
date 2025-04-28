{ stdenv, zig, raylib }:

stdenv.mkDerivation {
  pname = "template";
  version = "0.1.0";

  src = ./.;

  nativeBuildInputs = [ zig raylib ];

  buildPhase = ''
    zig build -Drelease-safe=true
  '';

  installPhase = ''
    zig build install --prefix $out
  '';
}
