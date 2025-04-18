{
  lib,
  config,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    inputs.nix-index-db.hmModules.nix-index

    ./setup.nix
    ../common/programs/vesktop
    ../../system/extras/mpd.nix

    ../common/programs/emacs

    ../common
    ../common/wayland
    ../common/wayland/hyprland
    # ../common/wayland/swayfx

    ../common/games

    #../common/games/lutris.nix
    #../common/games/steam.nix
    #../common/games/mihoyo.nix

    # ../common/wayland/hyprland/plugins/hyprbars.nix
    # ../common/wayland/hyprland/plugins/hyprexpo.nix

    ../common/programs/terminal/wezterm.nix
    ../common/programs/browser/chromium.nix
    ../common/programs/browser/zen.nix
    # ../common/programs/pass.nix
  ];

  home.file = {
    ".ssh/known_hosts".source = ../common/ssh/known_hosts;
    ".ssh/id_ed25519.pub".source = ../common/ssh/id_ed25519.pub;
    ".config/qmk/qmk.ini".source = pkgs.writeText "qmk.ini" ''
      [user]
      qmk_home = /home/flakes/Programming/qmk_userspace/qmk_firmware
    '';
    ".ssh/config".source = pkgs.writeText "config" ''
      AddKeysToAgent yes
    '';
    ".config/xdg-desktop-portal/portals.conf".source = pkgs.writeText "portals.conf" ''
      [preferred]
      default=hyprland;kde;gtk
      org.freedesktop.impl.portal.FileChooser=kde
    '';
    ".config/fish/completions/ns.fish".source = pkgs.writeText "ns.fish" ''
      function __nixpkgs_completions
          cat ~/Flake/bin/nixpkgs_list
      end
      complete -c ns -f -a "(__nixpkgs_completions)"
    '';
    ".config/fish/completions/nix-run.fish".source = pkgs.writeText "nix-run.fish" ''
      function __nixpkgs_completions
          cat ~/Flake/bin/nixpkgs_list
      end
      complete -c nix-run -f -a "(__nixpkgs_completions)"
    '';
  };

  home.packages = with pkgs; [
    obs-studio
    mangohud

    # mkxp-z

    eb-garamond
    blender

    deluge

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
    sessionPath = ["$HOME/Flakes/bin"];
    persistence = {
      "/persist/home/flakes" = {
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
      NNN_BMS = "p:$HOME/Programming;f:$HOME/Flake;c:$HOME/.config;w:/mnt/Windows";
      SPLIT = "v";
      GTK_THEME = "${config.gtk.theme.name}:dark";
      EDITOR = lib.mkDefault "hx";
      # TERMINAL = "kitty -1 --listen-on=unix:@mykitty";
      # BROWSER = "firefox";
      FILEBROWSER = "dolphin";
      TERMBROWSER = "n";
      WINEPREFIX = "$HOME/Games/Wine-Prefix";
    };
  };

  monitors = [
    {
      name = "DP-1";
      width = 2560;
      height = 1440;
      refreshRate = 144;
      x = 0;
      y = 0;
      workspace = "1";
      bind = [1 2 3 4 5];
    }
  ];
}
