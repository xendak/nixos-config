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
    # nautilus
    # nautilus-open-any-terminal
    kdePackages.dolphin
    kdePackages.ark
    kdePackages.kio-extras
    kdePackages.ffmpegthumbs
    kdePackages.kdegraphics-thumbnailers
  ];

  # to enable qtct colors
  # [UiSettings]
  # ColorScheme=*
  # QT_QPA_PLATFORMTHEME=kde dolphin

  home.persistence = {
    "/persist" = {
      directories = [ ".local/share/dolphin" ];
      files = [
        ".config/dolphinrc"
        ".local/state/dolphinstaterc"
      ];
    };
  };
}
