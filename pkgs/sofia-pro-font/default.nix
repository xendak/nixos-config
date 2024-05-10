{
  lib,
  pkgs,
  stdenv,
}:
with lib;
  stdenv.mkDerivation {
    name = "sofia-pro-font";
    version = "1.0";
    src = ./SofiaPro.ttf;

    dontUnpack = true;
    dontBuild = true;
    dontConfigure = true;

    installPhase = ''
      install -Dm 644 $src $out/share/fonts/ttf/SofiaPro.ttf
    '';

    meta = {
      description = "Adobe's Sofia Pro Font";
      license = licenses.unfree;
      platforms = platforms.all;
    };
  }
