{ config, lib, pkgs, user, ... }:
{
  home.packages = with pkgs; [
    #cinnamon.nemo
    #gnome.nautilus
    libsForQt5.dolphin
    libsForQt5.ark
  ];
  home.file."${config.xdg.configHome}/Kvantum" = {
    source = config.lib.file.mkOutOfStoreSymlink "/persist/snow/flakes/Flake/snow/common/programs/Kvantum";
  };
  home.persistence = {
    "/persist/snow/flakes" = {
      allowOther = true;
      directories = [ ".local/share/dolphin" ];
      files = [ ".config/dolphinrc" ];
    };
  };
}

