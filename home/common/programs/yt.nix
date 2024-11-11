{
  pkgs,
  config,
  ...
}: {
  home.packages = with pkgs; [
    youtube-music
    yt-dlp
    mpv
    mpd
    ncmpcpp
    waylyrics
  ];
  home.persistence = {
    "/persist/home/${config.home.username}" = {
      directories = [".config/YouTube Music"];
      allowOther = true;
    };
  };
}
