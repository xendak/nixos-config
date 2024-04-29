{ }: {
  imports = [
    ./programs
    ./hyprland
    ./qt.nix
    ./gtk.nix
    ./xdg.nix
    ./font.nix
    ./pass.nix
  ];
  programs.command-not-found.enable = false;
}
