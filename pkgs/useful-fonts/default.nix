{
  lib,
  stdenv,
}:
with lib;
  stdenv.mkDerivation {
    name = "useful-fonts";
    version = "1.0";
    src = ./SofiaPro.ttf;

    dontUnpack = true;
    dontBuild = true;
    dontConfigure = true;

    installPhase = ''
      install -Dm 644 $src $out/share/fonts/opentype/FOT-NEWRODINPRO-M.OTF
      install -Dm 644 $src $out/share/fonts/opentype/FOT-RODINHIMAWARIPRO-M.OTF
      install -Dm 644 $src $out/share/fonts/opentype/RODINPRO-B.OTF
      install -Dm 644 $src $out/share/fonts/ttf/PROGGYCLEANNERDFONTMONO-REGULAR.TTF
      install -Dm 644 $src $out/share/fonts/ttf/PROGGYCLEANSZNERDFONTMONO-REGULAR.TTF
      install -Dm 644 $src $out/share/fonts/ttf/PROGGYDOTTED_REGULAR.TTF
      install -Dm 644 $src $out/share/fonts/ttf/SofiaPro.ttf
    '';

    meta = {
      description = "Useful fonts i use somewhere";
      license = licenses.unfree;
      platforms = platforms.all;
    };
  }
