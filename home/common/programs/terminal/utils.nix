{
  pkgs,
  config,
  inputs,
  ...
}:
{
  home.packages = with pkgs; [
    libnotify
    gsettings-desktop-schemas
    gsettings-qt

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
    p7zip-rar
    xdg-utils
    bat
    lazygit
    tree

    imhex
    hexyl
    difftastic
    bitwise

    # DEFAULT LANGUAGES i use mostly?
    zig-master
    zls-overlay
    valgrind
    clang-tools
    llvmPackages_latest.libstdcxxClang
    llvmPackages_latest.libcxx
    llvmPackages_latest.lldb
    cppcheck
    gdb

    inputs.uwu-colors.packages.${pkgs.system}.default

    # language formatters
    # nodePackages.prettier
    # dprint
    # deno

    nixd # Nix LSP
    alejandra
    nixfmt-rfc-style # Nix formatter
    nvd # Differ
    nix-output-monitor
    nix-tree
    nh # Nice wrapper for NixOS and HM

    ltex-ls # Spell checking LSP
  ];

  # icon entry for bottom
  home.file = {
    ".local/share/applications/bottom.desktop".source =
      pkgs.writeText "bottom.desktop"
        # ini
        ''
          [Desktop Entry]
          Name=bottom
          Version=1.5
          GenericName=System Monitor
          Comment=A customizable cross-platform graphical process/system monitor for the terminal.
          Exec=btm
          Icon=/home/${config.home.username}/Flake/home/common/icons/bottom.svg
          Terminal=true
          Type=Application
          Categories=System;ConsoleOnly;Monitor;
          StartupNotify=false

        '';
  };

  home.persistence."/persist".directories = [
    ".local/cache/nix"
    ".config/imhex"
    ".local/share/imhex"
  ];

  programs.fzf = {
    enable = true;
    defaultOptions = [ "--color 16" ];
  };

  # enable dir-env integration
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
}
