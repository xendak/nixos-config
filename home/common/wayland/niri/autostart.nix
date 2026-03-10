{
  config,
  host,
  ...
}:
let
  home = config.home.username;
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
    { command = [ "nix-theme-starter" ]; }
    { command = [ "xwayland-satellite" ]; }
    {
      command = [
        "fish"
        "${bin_path}/sync-desktop-files.fish"
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
