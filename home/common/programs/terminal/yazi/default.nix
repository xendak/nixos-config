{
  pkgs,
  lib,
  ...
}:
let
  augment-command = pkgs.fetchFromGitHub {
    # https://github.com/hankertrix/augment-command.yazi
    owner = "hankertrix";
    repo = "augment-command.yazi";
    rev = "681158d8a088b30ee947c97b864ce300346d9965";
    hash = "sha256-ct0ieBbTmQb3LlSWP+txuzmKuugyDRVprFGCvhLOzYQ=";
  };
  faster-piper = pkgs.fetchFromGitHub {
    # https://github.com/alberti42/faster-piper.yazi
    owner = "alberti42";
    repo = "faster-piper.yazi";
    rev = "8b794bfa3bc9c780e3f03b6f5a0ccde7744e54bb";
    hash = "sha256-m6ZiwA36lcdZORK3KIz4Xq3bs7mmtC6j62B/+BuDGAQ=";
  };
  epub-thumb = pkgs.fetchFromGitHub {
    # https://github.com/kirasok/epub-preview.yazi
    owner = "kirasok";
    repo = "epub-preview.yazi";
    rev = "2e8079e4a7f6315de99a5b968ed5fda479f1f39c";
    hash = "sha256-00000000000000000000000000000000000000000000";
  };
in
{
  imports = [
    ./keybinds.nix
    ./settings.nix
  ];

  # TODO: Implement a TAG system later.. no plugins exist that satisfy my needs
  programs.yazi = {
    enable = true;
    package = pkgs.yazi;
    shellWrapperName = "yy";

    extraPackages = [
      pkgs.jq
      pkgs.duckdb
      pkgs.fd
      pkgs.zoxide
      pkgs.resvg
      pkgs.imagemagick
      pkgs.fzf
      pkgs.mediainfo
      pkgs.poppler
      pkgs.exiftool
      pkgs.glow
      pkgs.eza
      pkgs.bat
      pkgs.ffmpeg
      pkgs.zip
      pkgs.ripgrep
      pkgs.resvg
      pkgs.imagemagick
      pkgs.wl-clipboard
      pkgs.epub-thumbnailer
      (pkgs._7zz.override { enableUnfree = true; })
    ];

    plugins = {
      faster-piper = faster-piper;
      augment-command = augment-command;
      sudo = pkgs.yaziPlugins.sudo;
      smart-filter = pkgs.yaziPlugins.smart-filter;
      mediainfo = pkgs.yaziPlugins.mediainfo;
      duckdb = pkgs.yaziPlugins.duckdb;
      epub = epub-thumb;
    };

  };

  # for custom
  xdg.configFile."yazi/plugins" = {
    source = ./plugins;
    recursive = true;
  };

  xdg.mimeApps.defaultApplications = {
    "inode/directory" = lib.mkForce [ "yazi.desktop" ];
  };

  home.sessionVariables = {
    YAZI_ZOXIDE_OPTS = "--height=70% --margin=2% --padding=1% --border=rounded --info=default --layout=default --no-preview --ansi --no-sort";
  };
}
