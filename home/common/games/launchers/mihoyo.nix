{ config, ... }:
{
  home.persistence = {
    "/persist/home/${config.home.username}" = {
      allowOther = true;
      directories = [
        ".local/share/anime-game-launcher"
        ".local/share/honkers-railway-launcher"
      ];
    };
  };
}
