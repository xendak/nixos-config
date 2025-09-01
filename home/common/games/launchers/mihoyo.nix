{ config, ... }:
{
  home.persistence = {
    "/persist" = {
      directories = [
        ".local/share/anime-game-launcher"
        ".local/share/honkers-railway-launcher"
      ];
    };
  };
}
