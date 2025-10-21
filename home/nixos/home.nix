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

    ../setup.nix

    ../common/programs/emacs

    ../common/font.nix
    ../common/xdg.nix
    ../common/gtk.nix
    ../common/programs/terminal/git.nix
    ../common/programs/terminal/fish.nix

    ../common/programs/terminal/nushell

    ../common/programs/terminal/starship.nix
    ../common/programs/terminal/utils.nix
    ../common/programs/zathura.nix
    ../common/programs/terminal/nnn
    ../common/programs/terminal/nvim
    ../common/programs/terminal/helix
    ../common/programs/terminal/yazi
  ];

  home.packages = with pkgs; [
    qmk
    wally-cli
    fzf
  ];

  programs = {
    home-manager.enable = true;
    zoxide.enable = true;
  };

  # Home --------------------
  home = {
    username = lib.mkDefault "nixos";
    persistence = lib.mkForce { };
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
      EDITOR = "hx";
      BROWSER = "zen";
      FILEBROWSER = "dolphin";
      TERMBROWSER = "yazi";
      WINEPREFIX = "$HOME/Games/Wine-Prefix";
    };
  };

}
