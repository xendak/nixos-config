{ pkgs, lib, ... }:
{
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
