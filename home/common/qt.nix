{ pkgs, ...}:
{
  # xdg.configFile."Kvantum/kvantum.kvconfig".text = '' '';
  home.sessionVariables = {
    QT_STYLE_OVERRIDE = "kvantum";
  };

  qt = {
    enable = true;
    platformTheme.name = "gtk";
    style.name = "kvantum";
    style.package = with pkgs.stable; [
      libsForQt5.qtstyleplugin-kvantum
      qt6Packages.qtstyleplugin-kvantum
    ];
  };
}
