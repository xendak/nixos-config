{
  pkgs ? import <nixpkgs> { },
}:
{
  # an-anime-game-launcher-gtk-bin = pkgs.callPackage ./an-anime-game-launcher-gtk-bin { };
  useful-fonts = pkgs.callPackage ./useful-fonts { };
  #eb-garamond-font = pkgs.callPackage ./eb-garamond-font { };
  orchis-theme = pkgs.callPackage ./orchis-theme { };
  win10sur = pkgs.callPackage ./win10sur { };
  # breezex-cursor = pkgs.callPackage ./breezex-cursor { };
  mkxp-z = pkgs.callPackage ./mkxp-z { };
  # yuzu-updated = pkgs.callPackage ./yuzu-updated { };
  nix-inspect = pkgs.callPackage ./nix-inspect { };
  wl-ocr = pkgs.callPackage ./wl-ocr { };
  custom-xdg-desktop-portal-termfilechooser =
    pkgs.callPackage ./xdg-desktop-portal-termfilechooser
      { };
  suyu = pkgs.callPackage ./suyu-bin { };
}
