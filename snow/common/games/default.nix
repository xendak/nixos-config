{ pkgs, ... }: {
  imports = [
    ./lutris.nix
    ./an-anime-game-launcher-gtk-bin.nix
    ./steam.nix
  ];
  home.packages = with pkgs; [ gamescope ];

	xdg.mimeApps.defaultApplications = {
		"application/x-ms-dos-executable" = [ "wine.desktop" ];
	};
}
