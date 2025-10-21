{
  lib,
  config,
  pkgs,
  inputs,
  ...
}:
{
  imports = [
    inputs.nix-index-db.homeModules.nix-index

    ../setup.nix
    ../common/programs/vesktop
    ../../system/extras/mpd.nix

    ../common/programs/emacs

    ../common
    ../common/wayland
    # ../common/wayland/hyprland
    ../common/wayland/niri
    ../common/programs/terminal/nushell
    # ../common/wayland/swayfx

    ../common/games

    #../common/games/lutris.nix
    #../common/games/steam.nix
    #../common/games/mihoyo.nix

    # ../common/wayland/hyprland/plugins/hyprbars.nix
    # ../common/wayland/hyprland/plugins/hyprexpo.nix

    ../common/programs/obsidian
    ../common/programs/quickshell
    ../common/programs/terminal/wezterm.nix
    ../common/programs/terminal/foot.nix
    ../common/programs/browser/chromium.nix
    ../common/programs/browser/zen.nix
    # ../common/programs/pass.nix
  ];

  home.packages = with pkgs; [
    obs-studio
    mangohud

    # mkxp-z
    xdg-desktop-portal-termfilechooser

    eb-garamond
    blender

    deluge

    lact

    # keyboard
    qmk
    wally-cli
  ];

  programs = {
    home-manager.enable = true;
  };

  services.udiskie.enable = true;
  services.playerctld.enable = true;
  systemd.user.startServices = "sd-switch";

  # Home --------------------
  home = {
    username = lib.mkDefault "flakes";
    homeDirectory = lib.mkDefault "/home/flakes/";
    stateVersion = lib.mkDefault "24.05";
    sessionPath = [ "$HOME/Flakes/bin" ];
    persistence = {
      "/persist" = {
        directories = [
          "Flake"
          "Downloads"
          "Music"
          "Videos"
          "Programming"
          "Pictures"
          "Documents"
          ".config/fcitx5"
          ".config/OpenRGB"
          ".config/dconf"
          # ".local/share/anime-game-launcher"
          # ".local/share/honkers-railway-launcher"
          # ".nixops"
          ".local/share/direnv"
          # ".local/share/keyrings"
          ".local/share/Terraria"
          ".local/share/fonts"
          ".local/state/wireplumber"
          ".local/share/ssh"
          ".local/share/fish"
        ];
      };
    };
    sessionVariables = {
      UserKnownHostsFile = "$HOME/.local/share/ssh";
      SCREENSHOT_DIR = "$HOME/Pictures/Screenshots";
      XCURSOR_PATH = "${config.gtk.cursorTheme.package}/share/icons/:$XCURSOR_PATH";
      FULLSCREEN_SAVE_FILE = "$(date +%Y-%m-%d_%M).png";
      AREA_SAVE_FILE = "$(date +%Y-%m-%d_%M)_snip.png";
      AREA_CONFIG_DIR = "Snips";
      NNN_BMS = "p:$HOME/Programming;f:$HOME/Flake;c:$HOME/.config;w:/mnt/Windows";
      SPLIT = "v";
      GTK_THEME = "${config.gtk.theme.name}:dark";
      EDITOR = "hx";
      SUDO_EDITOR = "vi";
      # TERMINAL = "kitty -1 --listen-on=unix:@mykitty";
      BROWSER = "zen";
      FILEBROWSER = "dolphin";
      TERMBROWSER = "yazi";
      WINEPREFIX = "/home/flakes/Games/Wine-Prefix";
    };
  };

  monitors = [
    {
      name = "DP-1";
      width = 2560;
      height = 1440;
      refreshRate = 144.006;
      x = 0;
      y = 0;
      workspace = "1";
      bind = [
        1
        2
        3
        4
        5
      ];
    }
  ];
}
