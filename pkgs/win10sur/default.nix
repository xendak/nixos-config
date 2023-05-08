{ lib
, stdenvNoCC
, gtk3
, hicolor-icon-theme
, numix-icon-theme-circle
, jdupes
, fetchgit
, bash
, colorVariants ? [ ] # default: black blue
, circleFolders ?  false
, whitePanel ? false
}:

let pname = "win10sur";
in
  lib.checkListOfEnum "${pname}: theme variants" [ 
    "purple"
    "pink"
    "red"
    "blue"
    "green"
    "orange"
    "brown"
    "black"
    "a" # all colors 
  ] colorVariants

  stdenvNoCC.mkDerivation
  rec {
    inherit pname;
    version = "2022-05-18";


    src = fetchgit {
      url = "https://github.com/yeyushengfan258/Win10Sur-icon-theme.git";
      rev = "2f41287774dffb79946f41aa2a20b90ee22aa648";
      sha256 = "sha256-oCouCRjT7ajYLA4VJjVC7ufWTfBhUeeAgz8M6C/l4ac=";
    };

    nativeBuildInputs = [
      jdupes
      bash
      gtk3
    ];

    buildInputs = [
      hicolor-icon-theme
      numix-icon-theme-circle
    ];

    dontPatchELF = true;
    dontRewriteSymlinks = true;
    dontDropIconThemeCache = true;

    postPatch = ''
      patchShebangs install.sh
    '';

    installPhase = ''
      runHook preInstall

      mkdir -p $out/share/icons
      
      ./install.sh -d $out/share/icons \
        ${lib.optionalString (colorVariants != []) "-" + builtins.toString colorVariants} \
        ${lib.optionalString circleFolders "--circle"} \
        ${lib.optionalString whitePanel "--white"} 

      jdupes --link-soft --recurse $out/share
      mv $out/share/icons/win10sur-2022-05-18-dark $out/share/icons/Win10SurDark


      substituteInPlace $out/share/icons/Win10SurDark/index.theme --replace "Win10Sur" "Win10SurDark"

      runHook postInstall
    '';

    meta = with lib; {
      description = "Win10Sur Icon Theme";
      homepage = "https://github.com/yeyushengfan258/Win10Sur-icon-theme";
      license = licenses.gpl3Only;
      platforms = platforms.unix;
    };
  }
