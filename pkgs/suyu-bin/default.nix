{
  lib,
  stdenv,
}:

stdenv.mkDerivation rec {
  pname = "suyu";
  version = "0.0.4";

  src = ./suyu-${version}.tar.gz;

  dontBuild = true;
  dontConfigure = true;

  sourceRoot = ".";

  installPhase = ''
    runHook preInstall

    mkdir -p $out
    cp -r * $out/

    runHook postInstall
  '';

  # Add runtime dependencies if needed
  # buildInputs = [ qtbase libGL ];
  # nativeBuildInputs = [ makeWrapper ];

  # If you need to wrap binaries for runtime dependencies:
  # postFixup = ''
  #   wrapProgram $out/bin/suyu \
  #     --prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath [ libGL vulkan-loader ]}
  # '';

  meta = with lib; {
    description = "Suyu Nintendo Switch emulator (binary distribution)";
    homepage = "https://suyu.dev";
    license = licenses.gpl3Plus;
    platforms = platforms.linux;
    maintainers = [ maintainers.xendak ];
  };
}
