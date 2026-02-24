{
  pkgs,
  inputs,
  lib,
  ...
}:
let
  quickshell = inputs.quickshell.packages.${pkgs.stdenv.hostPlatform.system}.default;
in
{
  home.packages = [
    quickshell
    pkgs.cava
    pkgs.gtk3
    pkgs.kdePackages.qt5compat
    pkgs.qt6.qt5compat
    pkgs.imagemagick
    pkgs.swww
    pkgs.ddcutil
    pkgs.python3
    pkgs.python3Packages.numpy
    pkgs.python3Packages.pyaudio
    pkgs.app2unit
    pkgs.gnused
  ];

  home.sessionVariables.QML2_IMPORT_PATH = lib.concatStringsSep ":" [
    "${quickshell}/lib/qt-6/qml"
    "${pkgs.kdePackages.qtdeclarative}/lib/qt-6/qml"
    "${pkgs.kdePackages.kirigami.unwrapped}/lib/qt-6/qml"
  ];
}
