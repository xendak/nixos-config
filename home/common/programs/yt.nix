{
  pkgs,
  config,
  ...
}:
{
  home.packages = with pkgs; [
    youtube-music
    yt-dlp
    mpv
    mpd
    ncmpcpp
    waylyrics
  ];
  home.persistence = {
    "/persist" = {
      directories = [ ".config/YouTube Music" ];
    };
  };
}
