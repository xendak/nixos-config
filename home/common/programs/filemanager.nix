{ config, lib, pkgs, user, ... }:
{
  home.packages = with pkgs; [
    #cinnamon.nemo
    #gnome.nautilus
    libsForQt5.dolphin
    libsForQt5.ark
  ];

  home.persistence = {
    "/persist/home/${config.home.username}" = {
      allowOther = true;
      directories = [ ".local/share/dolphin" ];
      files = [ ".config/dolphinrc" ];
    };
  };
}

