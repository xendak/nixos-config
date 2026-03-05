{
  pkgs,
  ...
}:
{
  qt = {
    enable = true;
    platformTheme.name = "kde";
    style.package = with pkgs; [
      adwaita-qt
      adwaita-qt6
    ];
  };
  home.packages = with pkgs; [
    libsForQt5.qt5.qtwayland
    kdePackages.qtwayland
    transmission_4-qt6
    # kdePackages.qt6gtk2
  ];

}
