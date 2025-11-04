{ ... }:
{
  imports = [
    ./rofi # might put pw-rofi sometime there
    ./syncthing.nix
    # ./ags.nix
    ./discord.nix
    ./filemanager.nix
    ./anki.nix
    ./browser
    ./terminal # ./kitty.nix ./fish.nix ./git.nix ./nvim
    # ./kvantum
    ./zathura.nix
    ./yt.nix
    ./llm
  ];

  # TODO: maybe set this properly everywhere so i dont get annoyed by random shit not working
  xdg.mimeApps.defaultApplications = {
    "video/x-flv" = [ "mpv.desktop" ];
    "video/mp4" = [ "mpv.desktop" ];
    "application/x-mpegURL" = [ "mpv.desktop" ];
    "video/mp2t" = [ "mpv.desktop" ];
    "video/3gpp" = [ "mpv.desktop" ];
    "video/quicktime" = [ "mpv.desktop" ];
    "video/x-msvideo" = [ "mpv.desktop" ];
    "video/x-ms-wmv" = [ "mpv.desktop" ];
    "video/3gpp2" = [ "mpv.desktop" ];
    "application/octet-stream" = [ "mpv.desktop" ];
  };

}
