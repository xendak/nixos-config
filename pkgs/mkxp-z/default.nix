{
  lib,
  stdenv,
  fetchFromGitHub,
  physfs,
  openal,
  libvorbis,
  libogg,
  SDL2,
  ninja,
  SDL2_sound,
  SDL2_ttf,
  freetype,
  SDL2_image,
  pixman,
  libpng,
  libjpeg,
  zlib,
  libuchardet,
  meson,
  libiconv,
  xxd,
  git,
  libGL,
  pkg-config,
  libtheora,
  fluidsynth,
  cmake,
  ruby,
}:

stdenv.mkDerivation {
  pname = "mkxp-z";
  version = "unstable-2026-19-06";

  src = fetchFromGitHub {
    owner = "mkxp-z";
    repo = "mkxp-z";
    # https://github.com/mkxp-z/mkxp-z/commits/main
    rev = "80906e4785ec4cb39645d312d932d305f1d94bb5";
    sha256 = "sha256-TsfAaHWOtfvRB4tZJmV9a5V8XgsskOcSeEoq7qIIESg";
  };

  buildInputs = [
    cmake
    physfs
    openal
    libvorbis
    libogg
    libtheora
    fluidsynth
    SDL2
    SDL2_sound
    SDL2_ttf
    freetype
    SDL2_image
    pixman
    libpng
    libjpeg
    zlib
    libuchardet
    git
    libiconv
    ruby
    libGL
  ];

  nativeBuildInputs = [
    meson
    cmake
    libiconv
    ninja
    pkg-config
    xxd
    git
  ];

  postPatch = ''
    # Use Meson's built-in handler for iconv
    sed -i -E "s/compilers\['cpp'\]\.find_library\('iconv'[^)]*\)/dependency('iconv')/g" src/meson.build

    # Mock the charset dependency since it is already provided by glibc on Linux
    sed -i -E "s/compilers\['cpp'\]\.find_library\('charset'[^)]*\)/declare_dependency()/g" src/meson.build

    # Fix compatibility with newer SDL2_ttf versions
    find src -type f -exec sed -i 's/_TTF_Font/TTF_Font/g' {} +

    patchShebangs linux/
  '';

  postInstall = ''
    rm -rf $out/lib{,64}
  '';

  mesonFlags = [
    "-Dstatic_executable=false"
    "-Dworkdir_current=true"
    "-Dmri_version=${ruby.version.majMin}"
    "-Dcjk_fallback_font=true"
  ];

  NIX_CFLAGS_COMPILE = "-I${lib.getDev openal}/include/AL -I${lib.getDev SDL2_sound}/include/SDL2";
  NIX_LDFLAGS = "-ltheoradec";

  meta = with lib; {
    description = "RGSS on Steroids. With a ridiculous name.";
    homepage = "https://github.com/mkxp-z/mkxp-z";
    license = licenses.gpl2;
    platforms = platforms.linux;
  };
}
