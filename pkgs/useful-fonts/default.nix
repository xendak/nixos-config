{
  lib,
  stdenv,
}:
with lib;
  stdenv.mkDerivation {
    name = "useful-fonts";
    version = "1.0";
    src1 = ./SofiaPro.ttf;
    src2 = ./FOT-NEWRODINPRO-M.OTF;
    src3 = ./FOT-RODINHIMAWARIPRO-M.OTF;
    src4 = ./RODINPRO-B.OTF;
    src5 = ./PROGGYCLEANNERDFONTMONO-REGULAR.TTF;
    src6 = ./PROGGYCLEANSZNERDFONTMONO-REGULAR.TTF;
    src7 = ./PROGGYDOTTED_REGULAR.TTF;

    dontUnpack = true;
    dontBuild = true;
    dontConfigure = true;

    installPhase = ''
      install -Dm 644 $src2 $out/share/fonts/opentype/FOT-NEWRODINPRO-M.OTF
      install -Dm 644 $src3 $out/share/fonts/opentype/FOT-RODINHIMAWARIPRO-M.OTF
      install -Dm 644 $src4 $out/share/fonts/opentype/RODINPRO-B.OTF
      install -Dm 644 $src5 $out/share/fonts/ttf/PROGGYCLEANNERDFONTMONO-REGULAR.TTF
      install -Dm 644 $src6 $out/share/fonts/ttf/PROGGYCLEANSZNERDFONTMONO-REGULAR.TTF
      install -Dm 644 $src7 $out/share/fonts/ttf/PROGGYDOTTED_REGULAR.TTF
      install -Dm 644 $src1 $out/share/fonts/ttf/SofiaPro.ttf
    '';

    meta = {
      description = "Useful fonts i use somewhere";
      license = licenses.unfree;
      platforms = platforms.all;
    };
  }
