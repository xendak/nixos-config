{
  pkgs,
  ...
}:
let
  steam-with-pkgs = pkgs.steam.override {
    extraPkgs = pkgs: [
      pkgs.libxcursor
      pkgs.libxi
      pkgs.libxinerama
      pkgs.libxscrnsaver
      pkgs.libpng
      pkgs.libpulseaudio
      pkgs.libvorbis
      pkgs.stdenv.cc.cc.lib
      pkgs.libkrb5
      pkgs.keyutils
      pkgs.pango
      pkgs.libthai
      pkgs.harfbuzz
      pkgs.handlr-regex

      # FIX: trying out
      # xdg-utils
      (pkgs.writeShellScriptBin "xdg-open" ''handlr open "$@"'')
      (pkgs.writeShellScriptBin "xterm" "handlr launch x-scheme-handler/terminal -- \"$@\"")
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

        ".local/cache/mesa_shader_cache"
        ".local/cache/radv_builtin_shaders"
        ".config/MangoHud"
        ".local/share/umu"
        ".local/share/Steam"
        "Games/Steam"
      ];
    };
  };
}
