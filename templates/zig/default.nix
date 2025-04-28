{
  stdenv,
  zigPackage,
  raylib,
}:
stdenv.mkDerivation {
  pname = "template";
  version = "0.1.0";

  src = ./.;
  XDG_CACHE_HOME = "${placeholder "out"}";

  buildInputs = [raylib];
  nativeBuildInputs = [zigPackage];

  buildPhase = ''
    ${zigPackage}/bin/zig build
  '';

  installPhase = ''
    ${zigPackage}/bin/zig build install --prefix $out
    rm -rf $out/zig 
  '';
}
