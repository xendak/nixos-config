{ config, lib, pkgs, user, ... }:
{
  home.packages = with pkgs; [
    nautilus
    nemo
    dolphin
  ];
}

