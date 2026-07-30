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
  makeWrapper,
}:

stdenv.mkDerivation {
  pname = "mkxp-z";
  version = "unstable-2026-19-06";

  src = fetchFromGitHub {
    owner = "mkxp-z";
    repo = "mkxp-z";
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
    makeWrapper
  ];

  postPatch = ''
    sed -i -E "s/compilers\['cpp'\]\.find_library\('iconv'[^)]*\)/dependency('iconv')/g" src/meson.build
    sed -i -E "s/compilers\['cpp'\]\.find_library\('charset'[^)]*\)/declare_dependency()/g" src/meson.build
    find src -type f -exec sed -i 's/_TTF_Font/TTF_Font/g' {} +
    patchShebangs linux/
  '';

  postInstall = ''
    rm -rf $out/lib{,64}

    if [ -f "$out/bin/mkxp-z.x86_64" ]; then
      mv "$out/bin/mkxp-z.x86_64" "$out/bin/mkxp-z"
    fi

    mv "$out/bin/mkxp-z" "$out/bin/.mkxp-z-real"

    cat > "$out/bin/mkxp-z" << 'WRAPPER'
    #!/bin/sh
    RUBY_PATHS='["@ruby@/lib/ruby/@majMin@.0/x86_64-linux","@ruby@/lib/ruby/@majMin@.0"]'

    if [ -f mkxp.json ]; then
      tmp=$(mktemp)
      jq --argjson paths "$RUBY_PATHS" '.rubyLoadpath = ($paths + (.rubyLoadpath // [])) | unique' mkxp.json > "$tmp" && mv "$tmp" mkxp.json
    else
      jq -n --argjson paths "$RUBY_PATHS" '{"rubyLoadpath": $paths}' > mkxp.json
    fi

    exec "$(dirname "$0")/.mkxp-z-real" "$@"
    WRAPPER

    chmod +x "$out/bin/mkxp-z"
    substituteInPlace "$out/bin/mkxp-z" \
      --replace '@ruby@' '${ruby}' \
      --replace '@majMin@' '${ruby.version.majMin}'
  '';

  mesonFlags = [
    "-Dstatic_executable=false"
    "-Dworkdir_current=true"
    "-Dmri_version=${ruby.version.majMin}"
    "-Dcjk_fallback_font=true"
  ];

  NIX_CFLAGS_COMPILE = "-I${lib.getDev openal}/include/AL -I${lib.getDev SDL2_sound}/include/SDL2 -I${zlib.dev}/include";
  NIX_LDFLAGS = "-ltheoradec -L${zlib}/lib -lz";

  meta = with lib; {
    description = "RGSS on Steroids. With a ridiculous name.";
    homepage = "https://github.com/mkxp-z/mkxp-z";
    license = licenses.gpl2;
    platforms = platforms.linux;
  };
}
