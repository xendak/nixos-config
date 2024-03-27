{ pkgs, ... }:
{
  imports = [
    ./kitty.nix
    ./fish.nix
    ./waybar.nix
    #./mako.nix
    ./zathura.nix
    ./hyprpaper.nix
  ];

  home.packages = with pkgs; [
    hyprpaper
    grim
    pipewire
    slurp
    waypipe
    wf-recorder
    wl-clipboard
    wl-mirror
    pfetch
    ydotool
  ];

}
