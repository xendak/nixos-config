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

    ./setup.nix
    ./common/programs/vesktop
    ../system/extras/mpd.nix

    ./common/programs/emacs
    ./common/programs/hexecute.nix

    ./common
    ./common/wayland
    ./common/wayland/niri
    ./common/programs/terminal/nushell
    ./common/programs/obsidian
    ./common/programs/quickshell
    ./common/programs/terminal/wezterm
    ./common/programs/terminal/foot.nix
    ./common/programs/browser/zen.nix
  ];

  home.packages = with pkgs; [
    xdg-desktop-portal-termfilechooser
    deluge
    qmk
    wally-cli
  ];

  programs = {
    home-manager.enable = true;
  };

  services.udiskie.enable = true;
  services.playerctld.enable = true;
  systemd.user.startServices = "sd-switch";

  home = {
    username = lib.mkDefault "xendak";
    homeDirectory = lib.mkDefault "/home/xendak/";
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
          ".config/dconf"
          ".local/share/direnv"
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
      NNN_BMS = "p:$HOME/Programming;f:$HOME/Flake;c:$HOME/.config;w:/local/windows";
      SPLIT = "v";
      GTK_THEME = "${config.gtk.theme.name}:dark";
      EDITOR = "hx";
      SUDO_EDITOR = "hx";
      TERMINAL = lib.mkForce "foot";
      BROWSER = "zen-beta";
      FILEBROWSER = "dolphin";
      TERMBROWSER = "yazi";
      WINEPREFIX = "/home/xendak/Games/Wine-Prefix";
    };
  };
}
