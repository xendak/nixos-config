{...}: {
  imports = [
    ./programs
    ./qt.nix
    ./gtk.nix
    ./xdg.nix
    ./font.nix
  ];
  programs.command-not-found.enable = false;
}
