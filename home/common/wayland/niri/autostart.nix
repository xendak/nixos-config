{ pkgs, ... }:

{
  programs.niri.settings.spawn-at-startup = [
    {
      command = [
        "systemctl"
        "--user"
        "start"
        "hyprpolkitagent"
      ];
    }
    { command = [ "xwayland-satellite" ]; }
    { command = [ "theme-switcher" ]; }
    { command = [ "mkdir -p $HOME/tmp/Screenshots" ]; }
    { command = [ "sh $HOME/Flake/bin/bt-once.sh" ]; }
    { command = [ "${pkgs.networkmanager}/bin/nmcli radio wifi off" ]; }
    { command = [ "openrgb -d \"XPG Spectrix S40G\" -m Off" ]; }
    { command = [ "qs -c /home/flakes/Programming/probe/quickshell" ]; }
    {
      command = [
        "sh"
        "-c"
        "swww-daemon & swww img /home/flakes/Flake/home/common/wallpapers/13.jpg"
      ];
    }

  ];
}
