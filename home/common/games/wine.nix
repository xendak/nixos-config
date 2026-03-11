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
    wineWow64Packages.waylandFull
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
