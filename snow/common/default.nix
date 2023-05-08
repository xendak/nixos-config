{ pkgs, ... }: {
  imports = [
    ./games
    ./programs
    ./hyprland
    ./qt.nix
    ./gtk.nix
    ./xdg.nix
    ./font.nix
  ];
}
