{
  inputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    ./setup.nix

    ../common
    ../common/games/steam.nix
    ../common/programs/ags

    inputs.ags.homeManagerModules.default
    inputs.nix-index-db.hmModules.nix-index
  ];

  home.packages = with pkgs; [
    polkit_gnome
    mangohud
    gamescope
    mpv
    jq
    unzip
    unrar
    p7zip

    obs-studio

    bottom
    xdg-utils

    wineWowPackages.stable

    eb-garamond
    blender

    deluge
    pavucontrol
    playerctl

    hyprpaper
    grim
    pipewire
    slurp
    waypipe
    wf-recorder
    wl-clipboard
    wl-mirror
    pfetch
    ydotool

    # utils
    bat
    fzf
    ripgrep

    # keyboard
    qmk
    wally-cli
    clang-tools

    # ags
    swww
    sassc
    glib
    bun
    dart-sass
    fd
    inputs.matugen.packages.${system}.default
  ];

  programs = {
    home-manager.enable = true;
  };

  xdg.mimeApps.enable = true;
  xdg.mimeApps.defaultApplications = {
    "application/x-ms-dos-executable" = ["wine.desktop"];
  };

  i18n = {
    inputMethod = {
      enabled = "fcitx5";
      # fcitx.engines = with pkgs.fcitx-engines; [ mozc ];
      fcitx5.addons = with pkgs; [
        fcitx5-mozc
        fcitx5-gtk
        libsForQt5.fcitx5-qt
      ];
    };
  };

  services.udiskie.enable = true;
  services.playerctld.enable = true;
  systemd.user.startServices = "sd-switch";

  # Home --------------------
  home = {
    username = lib.mkDefault "drops";
    homeDirectory = lib.mkDefault "/home/drops/";
    stateVersion = lib.mkDefault "23.05";
    sessionPath = ["$HOME/Flake/bin"];
    persistence = {
      "/persist/home/drops" = {
        directories = [
          "Flake"
          "Downloads"
          "Music"
          "Videos"
          "Programming"
          "Pictures"
          "Documents"
          ".config/fcitx5"
          ".config/dconf"
          # ".local/share/anime-game-launcher"
          # ".local/share/honkers-railway-launcher"
          # ".nixops"
          # ".local/share/direnv"
          # ".local/share/keyrings"
          ".local/share/Terraria"
          ".local/share/fonts"
          ".local/state/wireplumber"
          ".local/share/ssh"
        ];
        allowOther = true;
      };
    };
    sessionVariables = {
      UserKnownHostsFile = "$HOME/.local/share/ssh";
      SCREENSHOT_DIR = "$HOME/Pictures/Screenshots";
      XCURSOR_PATH = "${config.gtk.cursorTheme.package}/share/icons/:$XCURSOR_PATH";
      FULLSCREEN_SAVE_FILE = "$(date +%Y-%m-%d_%M).png";
      AREA_SAVE_FILE = "$(date +%Y-%m-%d_%M)_snip.png";
      AREA_CONFIG_DIR = "Snips";
      NNN_TMPFILE = "$XDG_CONFIG_HOME/.config/nnn/.lastd";
      NNN_FIFO = "/tmp/nnn.fifo";
      NNN_BMS = "p:$HOME/Programming;f:$HOME/Flake;c:$HOME/.config;w:/mnt/Windows";
      NNN_PLUG = "p:preview-tui;f:fzcd";
      SPLIT = "v";
      GTK_THEME = "${config.gtk.theme.name}:dark";
      EDITOR = "nvim";
      TERMINAL = "kitty -1 --listen-on=unix:@mykitty";
      BROWSER = "firefox";
      FILEBROWSER = "dolphin";
      TERMBROWSER = "n";
      NIX_AUTO_RUN = "1";
      MOZ_ENABLE_WAYLAND = 1;
      ANKI_WAYLAND = "1";
      NIXOS_OZONE_WL = "1";
      LIBSEAT_BACKEND = "logind";
      XCURSOR_SIZE = 36;
      QT_QPA_PLATFORM = "wayland;xcb";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = "0";
      WLR_NO_HARDWARE_CURSORS = 1;
      INPUT_METHOD = "fcitx";
      XIM_SERVERS = "fcitx";
      XMODIFIERS = "@im=fcitx";
      XMODIFIER = "@im=fcitx";
      GTK_IM_MODULE = "fcitx";
      QT_IM_MODULE = "fcitx";
      WINEPREFIX = "$HOME/Games/Wine-Prefix";
      XDG_CURRENT_DESKTOP = "GNOME"; # fixing QT no icon bullshit

      # DISPLAY = "";
      # WAYLAND_DISPLAY = "wayland-0";
    };
  };

  monitors = [
    {
      name = "eDP-1";
      width = 1920;
      height = 1080;
      refreshRate = 60;
      x = 0;
      y = 0;
      workspace = "1";
      bind = [1 2 3 4 5];
    }
    # {
    #   name = "DP-2";
    #   width = 2560;
    #   height = 1440;
    #   refreshRate = 144;
    #   x = 0;
    #   y = 0;
    #   workspace = "6";
    #   bind = [6 7];
    # }
  ];
}
