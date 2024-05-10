{pkgs, ...}: {
  home.packages = with pkgs; [
    ytmdl
    youtube-music
    youtube-dl
    mpv
    mpd
    ncmpcpp
  ];
  #home.persistence = {
  #    "/persist/home/${config.home.username}" = {
  #        directories = [ ".local/share/zoxide" ];
  #        allowOther = true;
  #    };
  #};
}
