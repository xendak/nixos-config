{
  inputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    inputs.nix-index-db.hmModules.nix-index

    # inputs.agenix.homeManagerModules.default

    ./setup.nix

    ../common
    ../common/wayland
    ../common/wayland/hyprland

    ../common/programs/vesktop

    # ../common/wayland/swayfx
    # ../common/wayland/hyprland/plugins/hyprbars.nix
    # ../common/wayland/hyprland/plugins/hyprexpo.nix

    ../common/games/wine.nix
    ../common/games/launchers/steam.nix
    ../common/games/emulators/retroarch.nix
    ../common/programs/pass.nix
    # ../common/programs/rustdesk.nix
  ];

  home.file = {
    ".ssh/known_hosts".source = ../common/ssh/known_hosts;
    ".ssh/id_ed25519.pub".source = ../common/ssh/id_ed25519.pub;
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
          bat ~/Flake/bin/nixpkgs_list
      end
      complete -c ns -f -a "(__nixpkgs_completions)"
    '';
    ".config/fish/completions/nix-run.fish".source = pkgs.writeText "nix-run.fish" ''
      function __nixpkgs_completions
          bat ~/Flake/bin/nixpkgs_list
      end
      complete -c nix-run -f -a "(__nixpkgs_completions)"
    '';
  };

  home.packages = with pkgs; [
    obs-studio

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
    username = lib.mkDefault "drops";
    homeDirectory = lib.mkDefault "/home/drops/";
    stateVersion = lib.mkDefault "24.05";
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
      NNN_BMS = "p:$HOME/Programming;f:$HOME/Flake;c:$HOME/.config;w:/mnt/Windows";
      SPLIT = "v";
      GTK_THEME = "${config.gtk.theme.name}:dark";
      EDITOR = "nvim";
      TERMINAL = "kitty -1 --listen-on=unix:@mykitty";
      BROWSER = "firefox";
      FILEBROWSER = "dolphin";
      TERMBROWSER = "n";
      WINEPREFIX = "$HOME/Games/Wine-Prefix";
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
  ];
}
