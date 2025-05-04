{
  lib,
  config,
  pkgs,
  inputs,
  ...
}:
{
  imports = [
    inputs.nix-index-db.hmModules.nix-index

    ./setup.nix

    ../common/programs/emacs

    ../common/font.nix
    ../common/xdg.nix
    ../common/gtk.nix
    ../common/programs/terminal/git.nix
    ../common/programs/terminal/fish.nix
    ../common/programs/terminal/starship.nix
    ../common/programs/zathura.nix
    ../common/programs/terminal/nnn
    ../common/programs/terminal/nvim
    ../common/programs/terminal/helix
    ../common/programs/terminal/yazi
  ];

  home.file = {
    ".ssh/known_hosts".source = ../common/ssh/known_hosts;
    ".ssh/id_ed25519.pub".source = ../common/ssh/id_ed25519.pub;
    ".config/qmk/qmk.ini".source = pkgs.writeText "qmk.ini" ''
      [user]
      qmk_home = /home/nixos/Programming/qmk_userspace/qmk_firmware
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
    qmk
    wally-cli

    fzf
    bc # Calculator
    bottom # System viewer
    ncdu # TUI disk usage
    eza # Better ls
    ripgrep # Better grep
    fd # Better find
    httpie # Better curl
    diffsitter # Better diff
    jq # JSON pretty printer and manipulator
    unzip
    unrar
    p7zip
    xdg-utils
    lazygit
    tree
    bat

    clang-tools
    llvmPackages_latest.libstdcxxClang
    llvmPackages_latest.libcxx
    llvmPackages_latest.lldb
    cppcheck
    gdb
    inputs.uwu-colors.packages.${pkgs.system}.default

    nixd # Nix LSP
    alejandra
    nixfmt-rfc-style # Nix formatter
    nvd # Differ
    nix-output-monitor
    nh # Nice wrapper for NixOS and HM

    ltex-ls # Spell checking LSP
  ];

  programs = {
    home-manager.enable = true;
    zoxide.enable = true;
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
  };

  # Home --------------------
  home = {
    username = lib.mkDefault "nixos";
    homeDirectory = lib.mkDefault "/home/nixos/";
    stateVersion = lib.mkDefault "25.05";
    sessionPath = [ "$HOME/Flakes/bin" ];
    sessionVariables = {
      UserKnownHostsFile = "$HOME/.local/share/ssh";
      SCREENSHOT_DIR = "$HOME/Pictures/Screenshots";
      XCURSOR_PATH = "${config.gtk.cursorTheme.package}/share/icons/:$XCURSOR_PATH";
      FULLSCREEN_SAVE_FILE = "$(date +%Y-%m-%d_%M).png";
      AREA_SAVE_FILE = "$(date +%Y-%m-%d_%M)_snip.png";
      AREA_CONFIG_DIR = "Snips";
      NNN_BMS = "p:$HOME/Programming;f:$HOME/Flake;c:$HOME/.config;w:/mnt/c/Users/rggro";
      SPLIT = "v";
      GTK_THEME = "${config.gtk.theme.name}:dark";
      EDITOR = lib.mkDefault "hx";
      TERMINAL = "kitty -1 --listen-on=unix:@mykitty";
      BROWSER = "firefox";
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
