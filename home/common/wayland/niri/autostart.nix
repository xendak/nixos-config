{ config, pkgs, ... }:
let
  home = config.home.username;
  qs_path = "/home/${home}/Flake/home/common/programs/quickshell/niri";
  wallpaper = "/home/${home}/Flake/home/common/wallpapers/13.jpg";

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
    { command = [ "theme-switcher" ]; }
    {
      command = [
        "mkdir"
        "-p"
        "/home/${home}/tmp/Screenshots"
      ];
    }
    {
      command = [
        "qs"
        "-c"
        "${qs_path}"
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
        "/home/${home}/Flake/bin/bt-once.sh"
      ];
    }
    {
      command = [
        "${pkgs.networkmanager}/bin/nmcli"
        "radio"
        "wifi"
        "off"
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
    baseCommands ++ (if home == "flakes" then desktopCommands else [ ]);
}
