{
  config,
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    gamescope
    # wineWowPackages.stable
    # wineWowPackages.staging
    wineWowPackages.waylandFull
  ];

  home.persistence = {
    "/persist" = {
      directories = [
        "Games/Wine"
        "Games/Wine-Prefix"
      ];
    };
  };
}
