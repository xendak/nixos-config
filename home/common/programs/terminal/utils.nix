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
  home.packages = [
    pkgs.libnotify
    pkgs.gsettings-desktop-schemas
    pkgs.gsettings-qt

    pkgs.bc # Calculator
    pkgs.bottom # System viewer
    pkgs.ncdu # TUI disk usage
    pkgs.eza # Better ls
    pkgs.ripgrep # Better grep
    pkgs.fd # Better find
    pkgs.httpie # Better curl
    pkgs.jq # JSON pretty printer and manipulator
    pkgs.unzip
    pkgs.unrar
    (pkgs._7zz.override { enableUnfree = true; })

    pkgs.bat
    pkgs.lazygit
    pkgs.tree
    pkgs.ffmpeg

    pkgs.imhex
    pkgs.hexyl
    pkgs.difftastic
    pkgs.bitwise

    pkgs.duckdb
    pkgs.glow
    pkgs.imv
    pkgs.sxiv

    # :c :cpp
    pkgs.valgrind
    pkgs.clang-tools
    pkgs.llvmPackages_latest.libstdcxxClang
    pkgs.llvmPackages_latest.libcxx
    pkgs.llvmPackages_latest.lldb
    pkgs.cppcheck
    pkgs.rr
    pkgs.gdb
    pkgs.gf # gdb front-end

    # :Odin
    pkgs.odin
    pkgs.ols
    # :Zig
    pkgs.zig-master
    pkgs.zls-overlay
    # :Rust
    rustToolchain
    # :Go
    pkgs.go
    pkgs.gopls
    pkgs.delve
    # :PUC
    # :javascript
    pkgs.nodejs
    pkgs.prettier
    pkgs.dprint
    pkgs.deno
    # :Java
    pkgs.jdk25
    pkgs.gradle
    pkgs.maven

    # TUI emacs-like compilation mode
    inputs.gobuild.packages.${pkgs.stdenv.hostPlatform.system}.default

    inputs.uwu-colors.packages.${pkgs.stdenv.hostPlatform.system}.default

    pkgs.nixd # Nix LSP
    pkgs.alejandra
    pkgs.nixfmt # Nix formatter
    pkgs.nvd # Differ
    pkgs.nix-output-monitor
    pkgs.nix-tree
    pkgs.nh # Nice wrapper for NixOS and HM

    pkgs.ltex-ls # Spell checking LSP
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
    # defaultOptions = [ "--color=base16" ];
  };

  home.sessionVariables = {
    FZF_DEFAULT_OPTS_FILE = "/home/${config.home.username}/.config/fzf/colors";
  };

  # enable dir-env integration
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
}
