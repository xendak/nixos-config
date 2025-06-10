{
  config,
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    #cinnamon.nemo
    #gnome.nautilus
    # kdePackages.dolphin
    # or
    # libsForQt5.dolphin
    kdePackages.dolphin
    kdePackages.ark
    kdePackages.kio-extras
    kdePackages.ffmpegthumbs
    kdePackages.kdegraphics-thumbnailers
  ];

  home.persistence = {
    "/persist/home/${config.home.username}" = {
      allowOther = true;
      directories = [ ".local/share/dolphin" ];
      files = [ ".config/dolphinrc" ];
    };
  };
}
