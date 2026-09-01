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
    rev = "352dc37dd737792370e86b09099c1551162a5d43";
    hash = "sha256-PN8GE18MGYqpg8EdtCAH1cMRy1lFX9nA3y3lanmhY6E";
  };
  faster-piper = pkgs.fetchFromGitHub {
    # https://github.com/alberti42/faster-piper.yazi
    owner = "alberti42";
    repo = "faster-piper.yazi";
    rev = "bb90261ce3952762b0de2d5720ea176615c1bbd9";
    hash = "sha256-a7/KTIoIU9idxhYmYFsp6/ezmiBK/mEYfEz9zqZZiEU=";
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
      pkgs.rich-cli
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
      pkgs.sshfs
      pkgs.imagemagick
      pkgs.wl-clipboard
      pkgs.gnome-epub-thumbnailer
      (pkgs._7zz.override { enableUnfree = true; })
    ];

    plugins = {
      sshfs = pkgs.yaziPlugins.sshfs;
      faster-piper = faster-piper;
      piper = pkgs.yaziPlugins.piper;
      augment-command = augment-command;
      sudo = pkgs.yaziPlugins.sudo;
      smart-filter = pkgs.yaziPlugins.smart-filter;
      mediainfo = pkgs.yaziPlugins.mediainfo;
      duckdb = pkgs.yaziPlugins.duckdb;
      rich-preview = pkgs.yaziPlugins.rich-preview;
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
