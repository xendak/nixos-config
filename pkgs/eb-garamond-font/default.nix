{ lib, pkgs, stdenv }:
with lib;

stdenv.mkDerivation {
  name = "eb-garamond-font";
  version = "1.0";
  src = ./EBGaramond.ttf;

  dontUnpack = true;
  dontBuild = true;
  dontConfigure = true;

  installPhase = ''
    install -Dm 644 $src $out/share/fonts/ttf/EBGaramond.ttf
  '';

  meta = {
    description = "EB Garamond Google Font";
    license = licenses.unfree;
    platforms = platforms.all;
  };
}
