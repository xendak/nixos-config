{ pkgs, config, inputs, ... }:
 
# let
#   aagl-gtk-on-nix = import (
#     builtins.fetchTarball {
#       url = "https://github.com/ezKEa/aagl-gtk-on-nix/archive/main.tar.gz";
#       sha256 = "sha256:1sidfdcwidy7px05004hlcs63rajkskr8rzprjagwy410xib47gi";
#     }
#   );
# 
# in
{

  # home.packages = [
  #   aagl-gtk-on-nix.an-anime-game-launcher
  #   aagl-gtk-on-nix.the-honkers-railway-launcher
  #   aagl-gtk-on-nix.honkers-launcher
  # ];

  home.persistence = {
    "/persist/snow/flakes" = {
      allowOther = true;
      directories = [
        ".local/share/anime-game-launcher"
        ".local/share/honkers-railway-launcher"
      ];
    };
  };
}
