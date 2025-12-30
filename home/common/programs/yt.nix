{
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    pear-desktop
    yt-dlp
    mpv
    mpd
    ncmpcpp
    waylyrics
    pavucontrol
    ytmdl
  ];
  home.persistence = {
    "/persist" = {
      directories = [ ".config/YouTube Music" ];
    };
  };
}
