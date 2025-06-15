{
  config,
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    #cinnamon.nemo
    # kdePackages.dolphin
    # or
    # libsForQt5.dolphin
    nautilus
    nautilus-open-any-terminal
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
