{
  config,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    gamescope
    # wineWowPackages.stable
    # wineWowPackages.staging
    wineWowPackages.waylandFull
  ];

  home.persistence = {
    "/persist/home/${config.home.username}/" = {
      allowOther = true;
      directories = [
        "Games/Wine"
        "Games/Wine-Prefix"
      ];
    };
  };
}
