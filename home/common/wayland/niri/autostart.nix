{ config, host, ... }:
let
  home = config.home.username;
  qs_path = "/home/${home}/Flake/home/common/programs/quickshell/niri";
  wallpaper = "/home/${home}/Flake/home/common/wallpapers/13.jpg";
  bin_path = "/home/${home}/Flake/bin/";

  baseCommands = [
    {
      command = [
        "systemctl"
        "--user"
        "start"
        "hyprpolkitagent"
      ];
    }
    { command = [ "swww-daemon" ]; }
    { command = [ "xwayland-satellite" ]; }
    {
      command = [
        "${bin_path}/nix-theme-starter"
        "gorgoroth"
      ];
    }
    {
      command = [
        "mkdir"
        "-p"
        "/home/${home}/tmp/Screenshots"
      ];
    }
    {
      command = [
        "fish"
        "${bin_path}/sync-desktop-files.fish"
      ];
    }

    {
      command = [
        "fish"
        "${qs_path}/wallpaper.fish"
        "-f"
        "${wallpaper}"
      ];
    }
  ];

  desktopCommands = [
    {
      command = [
        "sh"
        "${bin_path}/bt-once.sh"
      ];
    }
    {
      command = [
        "openrgb"
        "-d"
        "XPG Spectrix S40G"
        "-m"
        "Off"
      ];
    }
  ];
in
{
  programs.niri.settings.spawn-at-startup =
    baseCommands ++ (if host == "Snow" then desktopCommands else [ ]);
}
