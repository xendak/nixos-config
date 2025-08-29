{ config, ... }:
{
  home.persistence = {
    "/persist" = {
      allowOther = true;
      directories = [
        ".local/share/anime-game-launcher"
        ".local/share/honkers-railway-launcher"
      ];
    };
  };
}
