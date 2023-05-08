{ config, pkgs, inputs, outputs, lib, ...}:
{
	#programs.qt5ct.enable = true;
	#home.packages = [ pkgs.qt5ct ];
	home.sessionVariables = { 
	  QT_QPA_PLATFORMTHEME = "gtk2"; 
	  QT_STYLE_OVERRIDE = "gtk2";
	};

	xdg.configFile."qt5ct/qt5ct.conf".text = ''
		[Appearance]
		icon_theme=gtk
		style=gtk2
	'';

	qt = {
		enable = true;
		platformTheme = lib.mkForce "gtk";
		style.name = "gtk2";
	};
}

#graphite-kde-theme	
#{
#	qt = {
#		enable = true;
#		platformTheme = "gtk";
#	};
#}
