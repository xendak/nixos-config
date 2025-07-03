{ ... }:
{
  imports = [
    ./rofi # might put pw-rofi sometime there
    # ./ags.nix
    ./discord.nix
    ./filemanager.nix
    ./anki.nix
    ./browser
    ./terminal # ./kitty.nix ./fish.nix ./git.nix ./nvim
    # ./kvantum
    ./zathura.nix
    ./yt.nix
    ./llm
  ];
}
