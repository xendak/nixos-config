{
  config,
  pkgs,
  ...
}:
let
  steam-with-pkgs = pkgs.steam.override {
    extraPkgs =
      pkgs: with pkgs; [
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
        pango
        libthai
        harfbuzz
      ];
  };
in
{
  home.packages = [
    steam-with-pkgs
    pkgs.gamescope
    pkgs.protontricks
    pkgs.mangohud
    # steamtinkerlaunch
  ];

  home.persistence = {
    "/persist" = {
      directories = [
        # {
        #   directory = ".local/share/Steam";
        #   method = "symlink";
        # }

        # ".config/steamtinkerlaunch"
        ".local/share/Steam"
        "Games/Steam"
      ];
    };
  };
}
