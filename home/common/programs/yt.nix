{
  pkgs,
  config,
  ...
}: {
  home.packages = with pkgs; [
    ytmdl
    youtube-music
    youtube-dl
    mpv
    mpd
    ncmpcpp
    osdlyrics
  ];
  home.persistence = {
    "/persist/home/${config.home.username}" = {
      directories = [".config/YouTube Music"];
      allowOther = true;
    };
  };
}
