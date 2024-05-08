{ config, pkgs, lib, ... }:
let
  steam-with-pkgs = pkgs.steam.override {
    extraPkgs = pkgs: with pkgs; [
      xorg.libXcursor
      xorg.libXi
      xorg.libXinerama
      xorg.libXScrnSaver
      libpng
      libpulseaudio
      libvorbis
      stdenv.cc.cc.lib
      libkrb5
      keyutils
    ];
  };
in
{
  home.packages = with pkgs; [
    steam-with-pkgs
    gamescope
    protontricks
    # steamtinkerlaunch
  ];
  home.persistence = {
    "/persist/home/${config.home.username}" = {
      allowOther = true;
      directories = [
        {
          directory = ".local/share/Steam";
          method = "symlink";
        }
        # ".config/steamtinkerlaunch"
        "Games/Steam"
      ];
    };
  };
}
