{ pkgs, ... }: {
  # imports = [
  #   ./launchers
  #   ./emulators
  # ];

  home.packages = with pkgs; [
    # Wine
    wineWow64Packages.waylandFull
    winetricks

    # Appimages
    appimage-run
  ];

  home.persistence = {
    "/persist" = {
      directories = [
        "Games"
      ];
    };
  };

}
