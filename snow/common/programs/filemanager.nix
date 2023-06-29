{ config, lib, pkgs, user, ... }:
{
  home.packages = with pkgs; [
    cinnamon.nemo
    gnome.nautilus
    libsForQt5.dolphin
  ];
}

