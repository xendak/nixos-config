{ config, pkgs, inputs, outputs, lib, ...}:
# {
# 	home.sessionVariables = { 
# 	  QT_QPA_PLATFORMTHEME = "gtk2"; 
# 	  QT_STYLE_OVERRIDE = "gtk2";
# 	};
# 
# 	xdg.configFile."qt5ct/qt5ct.conf".text = ''
# 		[Appearance]
# 		icon_theme=gtk
# 		style=gtk2
# 	'';
# 
# 	qt = {
# 		enable = true;
# 		platformTheme = lib.mkForce "gtk";
# 		style.name = "gtk2";
# 	};
# }
{

  # xdg.configFile."Kvantum/kvantum.kvconfig".text = '' '';
  home.sessionVariables = {
    QT_STYLE_OVERRIDE = "kvantum";
  };

  qt = {
    enable = true;
    platformTheme.name = "gtk";
    style.name = "kvantum";
    style.package = with pkgs; [
      libsForQt5.qtstyleplugin-kvantum
      qt6Packages.qtstyleplugin-kvantum
    ];
  };
}
