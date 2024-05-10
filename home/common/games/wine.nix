{
  config,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    gamescope
    wineWowPackages.stable
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
