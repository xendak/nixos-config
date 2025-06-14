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
    nautilus
    nautilus-open-any-terminal
    libsForQt5.dolphin
    libsForQt5.ark
    libsForQt5.kio-extras
    libsForQt5.ffmpegthumbs
    libsForQt5.kdegraphics-thumbnailers
  ];

  home.persistence = {
    "/persist/home/${config.home.username}" = {
      allowOther = true;
      directories = [ ".local/share/dolphin" ];
      files = [ ".config/dolphinrc" ];
    };
  };
}
