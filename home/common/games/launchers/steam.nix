{
  pkgs,
  ...
}:
let
  steam-with-pkgs = pkgs.steam.override {
    extraPkgs =
      pkgs: with pkgs; [
        libxcursor
        libxi
        libxinerama
        libxscrnsaver
        libpng
        libpulseaudio
        libvorbis
        stdenv.cc.cc.lib
        libkrb5
        keyutils
        pango
        libthai
        harfbuzz
        xdg-utils
      ];
  };
in
{
  home.packages = [
    steam-with-pkgs
    pkgs.gamescope
    pkgs.protontricks
    pkgs.mangohud
    pkgs.umu-launcher
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
        ".config/MangoHud"
        ".local/share/umu"
        ".local/share/Steam"
        "Games/Steam"
      ];
    };
  };
}
