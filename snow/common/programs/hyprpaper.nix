{ config, lib, pkgs, user, ... }:
{
  home.packages = with pkgs; [
    hyprpaper
  ];

  xdg.configFile."hypr/hyprpaper.conf".text = ''
    preload = ~/Pictures/wallpaper1.png
    preload = ~/Pictures/wallpaper5.png
    wallpaper = DP-1,~/Pictures/wallpaper1.png
    wallpaper = DP-2,~/Pictures/wallpaper5.png
  '';


}
