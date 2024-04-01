{pkgs, ...}:
{
  imports = [
    ./rofi # might put pw-rofi sometime there
    ./discord.nix
    ./filemanager.nix
    ./anki.nix
    ./browser
    ./terminal # ./kitty.nix ./fish.nix ./git.nix ./nvim
    ./kvantum
    ./zathura.nix
  ];
}
