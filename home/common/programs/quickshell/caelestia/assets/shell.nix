{
  pkgs ? import <nixpkgs> { },
}:

pkgs.mkShell {
  buildInputs = [
    pkgs.pipewire
    pkgs.aubio
  ];

  nativeBuildInputs = [
    pkgs.pkg-config
    pkgs.gdb
  ];
  shellHook = ''
    export CPATH="${pkgs.pipewire.dev}/include/pipewire-0.3:${pkgs.pipewire.dev}/include/spa-0.2"

    echo "--> CPATH=$CPATH"
    echo "›› To compile, you can now run:"
    # echo '›› g++ -std=c++17 -O3 -Wall -Wextra -o beat_detector beat_detector.cpp $(pkg-config --cflags --libs libpipewire-0.3 aubio)'
    echo '>>
    clang++ -std=c++17 -O3 -Wall -Wextra -I/usr/include/pipewire-0.3 -I/usr/include/spa-0.2 -I/usr/include/aubio -o beat_detector beat-detector.cpp -lpipewire-0.3 -laubio -pthread'
  '';
}
