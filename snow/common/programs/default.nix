{pkgs, ...}:
{
  imports = [
    ./rofi # might put pw-rofi sometime there
    ./discord.nix
    ./filemanager.nix
    ./anki.nix
    ./browser
    ./terminal # ./kitty.nix ./fish.nix ./git.nix ./nvim
    #./mako.nix
    ./zathura.nix
    ./hyprpaper.nix
  ];
}
