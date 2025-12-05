{
  lib,
  stdenv,
  clangStdenv,
  fetchFromGitHub,
  cmake,
  pkg-config,
  gnumake,
  libGL,
  libGLU,
  xorg,
  glib,
  gtk2,
  dbus,
  atk,
  pango,
  cairo,
  gdk-pixbuf,
  harfbuzz,
  patchelf,
  makeDesktopItem,
  copyDesktopItems,
  miltonIcon ? ./milton.png,
}:

clangStdenv.mkDerivation rec {
  pname = "milton";
  version = "1.9.1";

  src = fetchFromGitHub {
    owner = "serge-rgb";
    repo = "milton";
    rev = "v${version}";
    hash = "sha256-BMz3Ov0AsFFmPyyULtAazEYkbVESw2OdE1TkydeRpvY=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [
    cmake
    pkg-config
    gnumake
    patchelf
    copyDesktopItems
  ];

  buildInputs = [
    libGL
    libGLU
    xorg.libX11
    xorg.libXext
    xorg.libXrandr
    xorg.libXinerama
    xorg.libXcursor
    xorg.libXi
    xorg.libXScrnSaver
    xorg.libXxf86vm
    glib
    gtk2
    dbus
    atk
    pango
    cairo
    gdk-pixbuf
    harfbuzz
  ];

  desktopItems = [
    (makeDesktopItem {
      name = "milton";
      desktopName = "Milton";
      exec = "milton";
      icon = "milton";
      comment = "Paint without pixels";
      categories = [ "Graphics" ];
    })
  ];

  hardeningDisable = [ "format" ];
  dontUseCmakeConfigure = true;

  postPatch = ''
    sed -i 's/cmake_minimum_required.*/cmake_minimum_required(VERSION 3.5)/' CMakeLists.txt
    sed -i 's/cmake_minimum_required.*/cmake_minimum_required(VERSION 3.5)/' third_party/SDL2-2.0.8/CMakeLists.txt
    sed -i 's/-std=c++[0-9][0-9]/-std=c++17/g' CMakeLists.txt
    sed -i 's/-Werror//g' CMakeLists.txt
  '';

  buildPhase = ''
    export ROOT_DIR=$PWD

    echo "--- Building Custom SDL2 ---"
    cd third_party/SDL2-2.0.8
    mkdir -p build
    cd build 

    cmake .. \
      -DVIDEO_WAYLAND=OFF \
      -DCMAKE_INSTALL_PREFIX="$PWD/linux64" \
      # -DCMAKE_BUILD_TYPE=Debug \
      # -DCMAKE_POLICY_VERSION_MINIMUM=3.5

    make -j$NIX_BUILD_CORES install

    # needs the "debug." for sme reason?
    cd linux64/lib
    ln -sf libSDL2.a libSDL2d.a
    ln -sf libSDL2main.a libSDL2maind.a

    cd ../include
    cp SDL2/* .

    echo "--- Building Milton ---"
    cd $ROOT_DIR
    mkdir -p build
    cd build

    cmake .. \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_CXX_STANDARD=17 \
      -DCMAKE_CXX_FLAGS="-std=c++17"
      
    make -j$NIX_BUILD_CORES
  '';

  installPhase =
    let
      runtimeDeps = [
        stdenv.cc.cc.lib
        libGL
        libGLU
        xorg.libX11
        xorg.libXext
        xorg.libXrandr
        xorg.libXinerama
        xorg.libXcursor
        xorg.libXi
        xorg.libXScrnSaver
        xorg.libXxf86vm
        glib
        gtk2
        dbus
        atk
        pango
        cairo
        gdk-pixbuf
        harfbuzz
      ];

      myDesktopItem = makeDesktopItem {
        name = "milton";
        desktopName = "Milton";
        exec = "milton";
        icon = "milton";
        comment = "Paint application";
        categories = [ "Graphics" ];
      };
    in
    ''
      mkdir -p $out/bin
      mkdir -p $out/share/pixmaps
      mkdir -p $out/share/applications
      cp ${miltonIcon} $out/share/pixmaps/milton.png

      ln -s ${myDesktopItem}/share/applications/*.desktop $out/share/applications/

      ls ./*

      cp Milton $out/bin/milton


      patchelf --set-rpath "${lib.makeLibraryPath runtimeDeps}" $out/bin/milton
    '';

  meta = with lib; {
    description = "Milton Paint";
    homepage = "https://github.com/serge-rgb/milton";
    license = licenses.gpl3;
    platforms = platforms.linux;
  };
}
