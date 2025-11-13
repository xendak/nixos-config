{
  lib,
  stdenv,
}:
with lib;
stdenv.mkDerivation {
  name = "useful-fonts";
  version = "1.0";

  src = builtins.filterSource (
    path: type:
    let
      cName = baseNameOf (toString path);
      baseName = lib.strings.toLower cName;
    in
    type == "directory"
    || (
      type == "regular"
      && (lib.strings.hasSuffix ".ttf" baseName || lib.strings.hasSuffix ".otf" baseName)
    )
  ) ./.;

  dontBuild = true;
  dontConfigure = true;

  installPhase = ''
    mkdir -p $out/share/fonts/truetype
    mkdir -p $out/share/fonts/opentype

    # Find and install TTF fonts
    find $src -iname "*.ttf" -type f -exec sh -c 'install -Dm 644 "$1" "$2/share/fonts/truetype/$(basename "$1")"' _ {} $out \;

    # Find and install OTF fonts  
    find $src -iname "*.otf" -type f -exec sh -c 'install -Dm 644 "$1" "$2/share/fonts/opentype/$(basename "$1")"' _ {} $out \;
  '';

  meta = {
    description = "Useful fonts i use somewhere";
    license = licenses.unfree;
    platforms = platforms.all;
  };
}
