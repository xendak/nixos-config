{
  config,
  pkgs,
  ...
}:
let
  grimblast = "grimblast";
  wpctl = "${pkgs.wireplumber}/bin/wpctl";
  brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl";
  # swaylock = "${config.programs.swaylock.package}/bin/swaylock";
  playerctl = "${config.services.playerctld.package}/bin/playerctl";

  baseterminal = config.home.sessionVariables.TERMINAL;
  terminal = if baseterminal == "wezterm" then baseterminal + " start " else baseterminal;
  browser = config.home.sessionVariables.BROWSER;
  # editor = config.home.sessionVariables.EDITOR;
  filebrowser = config.home.sessionVariables.FILEBROWSER;
  termbrowser = config.home.sessionVariables.TERMBROWSER;
  print = "$HOME/Pictures/Screenshots/$(date +%Y-%m-%d-%M)";
  tmpprint = "$HOME/tmp/Screenshots/$(date +%Y-%m-%d-%M-%S)";

in
{
  programs.niri.settings.binds =
    with config.lib.niri.actions;
    let
      volume-up = spawn wpctl [
        "set-sink-volume"
        "@DEFAULT_SINK@"
        "0.05+"
      ];
      volume-down = spawn wpctl [
        "set-sink-volume"
        "@DEFAULT_SINK@"
        "0.05-"
      ];
    in
    {
      "xf86audioraisevolume".action = volume-up;
      "xf86audiolowervolume".action = volume-down;

      "super+xf86audioraisevolume".action = spawn "${brightnessctl} set +5%";
      "super+xf86audiolowervolume".action = spawn "${brightnessctl} set 5%-";

      "super+q".action = close-window;
      "super+b".action = spawn browser;
      "super+Return".action = spawn terminal;
      "super+Shift+Return" = spawn terminal "--class f_terminal";

      "super+e".action = spawn termbrowser;
      "super+E".action = spawn filebrowser;

      "super+f".action = fullscreen-window;
      "super+t".action = toggle-window-floating;

      "super+r".action =
        spawn "rofi -show drun -matching fuzzy -sorting-method fzf -sort -drun-match-fields name,generic,categories -theme \"${config.xdg.configHome}/rofi/config.rasi\"";
      "super+x".action = spawn "rofi ${config.xdg.configHome}/rofi/powermenu.sh";

      "Print".action = spawn "${grimblast} --notify copysave output \"${print}_full.png\"";
      "Shift+Print".action = spawn "${grimblast} --notify copysave active \"${print}_active.png\"";
      "Alt+S".action = spawn "${grimblast} --notify copysave area \"${print}_snip.png\"";
      "Alt+C".action = spawn "${grimblast} --notify copysave area \"${tmpprint}_snip.png\"";

      "XF86AudioMicMute".action = spawn "${wpctl} set-mute @DEFAULT_SOURCE@ toggle";
      "XF86AudioPlay".action = spawn "${playerctl} play-pause";
      "XF86AudioStop".action = spawn "${playerctl} pause";
      "XF86AudioPause".action = spawn "${playerctl} pause";
      "XF86AudioPrev".action = spawn "${playerctl} previous";
      "XF86AudioNext".action = spawn "${playerctl} next";

      "super+Left".action = focus-column-left;
      "super+Right".action = focus-column-right;
      "super+Down".action = focus-workspace-down;
      "super+Up".action = focus-workspace-up;

      "super+Shift+Left".action = move-column-left;
      "super+Shift+Right".action = move-column-right;
      "super+Shift+Down".action = move-column-to-workspace-down;
      "super+Shift+Up".action = move-column-to-workspace-up;

      "super+2".action = focus-workspace "browser";
      "super+3".action = focus-workspace "vesktop";
    };
}
