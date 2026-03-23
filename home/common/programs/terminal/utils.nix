{
  pkgs,
  config,
  inputs,
  ...
}:
let
  rustToolchain = pkgs.rust-bin.stable.latest.default.override {
    extensions = [
      "rust-src"
      "rust-analyzer"
      "clippy"
      "rustfmt"
    ];
  };
in
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
    jq # JSON pretty printer and manipulator
    unzip
    unrar
    p7zip-rar

    bat
    lazygit
    tree
    ffmpeg

    imhex
    hexyl
    difftastic
    bitwise

    duckdb
    glow
    imv
    sxiv

    valgrind
    clang-tools
    llvmPackages_latest.libstdcxxClang
    llvmPackages_latest.libcxx
    llvmPackages_latest.lldb
    cppcheck
    rr
    gdb
    gf # gdb front-end

    # Programming Languages
    # Odin
    odin
    ols
    # Zig
    zig-master
    zls-overlay
    # Rust
    rustToolchain
    # Go
    go
    gopls
    delve
    # PUC
    # javascript
    nodejs
    nodePackages.prettier
    dprint
    deno
    # Java
    jdk25
    gradle
    maven

    inputs.uwu-colors.packages.${pkgs.stdenv.hostPlatform.system}.default

    nixd # Nix LSP
    alejandra
    nixfmt # Nix formatter
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
    ".config/bat/config".source =
      pkgs.writeText "config"
        # yaml
        ''
          --theme="base16"
          --map-syntax "*ignore:Git Ignore"
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
