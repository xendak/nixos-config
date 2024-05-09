{ inputs, pkgs, ... }:
{
  imports = [
    inputs.ags.homeManagerModules.default
  ];

  home.packages = [
    inputs.matugen.packages.${pkgs.system}.default

    pkgs.swww
    pkgs.glib
    pkgs.bun
    pkgs.dart-sass
    pkgs.fd

    pkgs.ollama
    pkgs.pywal
    pkgs.sassc
    (pkgs.python311.withPackages (p: [
      p.material-color-utilities
      p.pywayland
    ]))
  ];


  # home.file = {
  #   ".config/ags" = {
  #     recursive = true;
  #     enable = true;
  #     source = ./ags;
  #   };
  # };

  programs.ags = {
    enable = true;
    configDir = null; # if ags dir is managed by home-manager, it'll end up being read-only. not too cool.
    # configDir = ./.config/ags;

    extraPackages = with pkgs; [
      gtksourceview
      gtksourceview4
      ollama
      python311Packages.material-color-utilities
      python311Packages.pywayland
      pywal
      sassc
      webkitgtk
      webp-pixbuf-loader
      ydotool
    ];
  };
}
