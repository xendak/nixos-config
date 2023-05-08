{ lib
, fetchzip
, stdenv
}:

let
  _src = variant: suffix: hash: fetchzip ({
    name = variant;
    url = "https://github.com/ful1e5/BreezeX_Cursor/releases/download/v${version}/${variant}.${suffix}";
    hash = hash;
  } // (if suffix == "zip" then { stripRoot = false; } else {}));

  version = "2.0.0";
  srcs = [
    (_src "BreezeX-Black" "tar.gz" "sha256-5su79uUG9HLeAqXDUJa/VhpbYyy9gFj/VdtRPY0yUL4=")
    (_src "BreezeX-Light" "tar.gz" "sha256-72TXKX+q8gYwxGAZAFHfR5Q9xxlEj9a31GttevuLq8g=")
  ];
in stdenv.mkDerivation rec {
  pname = "breezex-cursor";
  inherit version;
  inherit srcs;

  sourceRoot = ".";

  installPhase = ''
    install -dm 0755 $out/share/icons
    cp -r BreezeX* $out/share/icons/
  '';

  meta = with lib; {
    description = "A cursor theme inspired by Breeze but with some modifications.";
    homepage = "https://github.com/ful1e5/BreezeX_Cursor";
    license = licenses.gpl3;
    platforms = platforms.linux;
    maintainers = with maintainers; [ xendak ];
  };
}
