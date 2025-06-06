{
  stdenv,
  lib,
  zigPackage,
  raylib,
}:
stdenv.mkDerivation {
  pname = "{{projectName}}";
  version = "0.1.0";

  src = ./.;
  XDG_CACHE_HOME = "${placeholder "out"}";

  buildInputs = [ raylib ];
  nativeBuildInputs = [ zigPackage ];

  buildPhase = ''
    ${zigPackage}/bin/zig build
  '';

  installPhase = ''
    ${zigPackage}/bin/zig build install --prefix $out
    rm -rf $out/zig
  '';

  meta = with lib; {
    description = "{{projectName}} - A Zig application";
    homepage = "https://github.com/xendak/{{projectName}}";
    license = licenses.mit;
    maintainers = [ maintainers.xendak ];
    platforms = platforms.all;
  };
}
