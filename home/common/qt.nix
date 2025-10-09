{
  pkgs,
  ...
}:
{
  qt = {
    enable = true;
    platformTheme.name = "qtct";
    style.package = with pkgs; [
      adwaita-qt
      adwaita-qt6
    ];
  };
  home.packages = with pkgs; [
    libsForQt5.qt5.qtwayland
    kdePackages.qtwayland
  ];

}
