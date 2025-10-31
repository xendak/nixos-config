{
  config,
  pkgs,
  ...
}:
let
  grimblast = "/home/${config.home.username}/Flake/bin/grimniri";
  hyprpicker = "${pkgs.hyprpicker}/bin/hyprpicker";
  wpctl = "${pkgs.wireplumber}/bin/wpctl";
  brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl";
  swaylock = "${config.programs.swaylock.package}/bin/swaylock";
  playerctl = "${config.services.playerctld.package}/bin/playerctl";
  wl-ocr-freeze = "/home/${config.home.username}/Flake/bin/wl-ocr-freeze";

  baseterminal = config.home.sessionVariables.TERMINAL;
  terminal =
    if baseterminal == "wezterm" then
      [
        "wezterm"
        "start"
      ]
    else
      baseterminal;
  browser = config.home.sessionVariables.BROWSER;
  # editor = config.home.sessionVariables.EDITOR;
  filebrowser = config.home.sessionVariables.FILEBROWSER;
  termbrowser = config.home.sessionVariables.TERMBROWSER;
  print = "/home/${config.home.username}/Pictures/Screenshots";
  tmpprint = "/home/${config.home.username}/tmp/Screenshots";

  qs-command = [
    "qs"
    "-c"
    "/home/${config.home.username}/Flake/home/common/programs/quickshell/niri"
    "ipc"
    "call"
    "shortcuts"
  ];
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

      "super+xf86audioraisevolume".action = spawn [
        "${brightnessctl}"
        "set"
        "+5%"
      ];
      "super+xf86audiolowervolume".action = spawn [
        "${brightnessctl}"
        "set"
        "5%-"
      ];

      "super+h".action = switch-preset-column-width;
      "super+Shift+h".action = switch-preset-window-height;

      "super+q".action = close-window;
      "super+w".action = spawn browser;
      "super+Return".action = spawn terminal;
      "super+Shift+Return".action = spawn terminal [
        "--class"
        "f_terminal"
      ];

      "super+Shift+e".action = spawn terminal [
        "--class"
        "f_yazi"
      ] termbrowser;

      "super+e".action = spawn filebrowser;

      "super+f".action = expand-column-to-available-width;
      "super+Shift+f".action = fullscreen-window;
      "super+Shift+Space".action = toggle-window-floating;

      "super+c".action = center-visible-columns;

      "super+z".action = toggle-column-tabbed-display;
      "super+j".action = focus-window-or-workspace-down;
      "super+k".action = focus-window-or-workspace-up;
      "super+Comma".action = consume-or-expel-window-left;
      "super+Period".action = consume-or-expel-window-right;

      "super+r".action = spawn qs-command [ "toggleLauncher" ];
      "super+d".action = spawn qs-command [ "toggleCmd" ];
      "super+o".action = spawn [ "${wl-ocr-freeze}" ];
      "Ctrl+Alt+c".action = spawn qs-command [ "clearNotifs" ];
      "Ctrl+Alt+Delete".action = spawn qs-command [ "toggleSession" ];
      "super+x".action = spawn qs-command [ "toggleSession" ];
      "super+p".action = spawn qs-command [ "toggleRbw" ];

      # "super+d".action = spawn [
      #   "rofi"
      #   "-show"
      #   "drun"
      #   "-matching"
      #   "fuzzy"
      #   "-sorting-method"
      #   "fzf"
      #   "-sort"
      #   "-drun-match-fields"
      #   "name,generic,categories"
      #   "-theme"
      #   "${config.xdg.configHome}/rofi/config.rasi"
      # ];

      "Alt+Shift+bracketleft".action.screenshot-window = [ ];
      "Print".action = spawn [
        "${grimblast}"
        "--freeze"
        "--notify"
        "copysave"
        "output"
        "${print}"
      ];
      "Shift+Print".action = spawn [
        "${grimblast}"
        "--freeze"
        "--notify"
        "copysave"
        "output"
        "${tmpprint}"
      ];
      "Alt+Shift+s".action = spawn [
        "${grimblast}"
        "--freeze"
        "--notify"
        "copysave"
        "area"
        "${print}"
      ];
      "Alt+Shift+c".action = spawn [
        "${grimblast}"
        "--freeze"
        "--notify"
        "copysave"
        "area"
        "${tmpprint}"
      ];

      "XF86AudioMicMute".action = spawn [
        "${wpctl}"
        "set-mute"
        "@DEFAULT_SOURCE@"
        "toggle"
      ];
      "XF86AudioPlay".action = spawn [
        "${playerctl}"
        "play-pause"
      ];
      "XF86AudioStop".action = spawn [
        "${playerctl}"
        "pause"
      ];
      "XF86AudioPause".action = spawn [
        "${playerctl}"
        "pause"
      ];
      "XF86AudioPrev".action = spawn [
        "${playerctl}"
        "previous"
      ];
      "XF86AudioNext".action = spawn [
        "${playerctl}"
        "next"
      ];

      "super+Left".action = focus-column-left;
      "super+Right".action = focus-column-right;
      "super+Down".action = focus-workspace-down;
      "super+Up".action = focus-workspace-up;

      "super+Shift+Left".action = move-column-left;
      "super+Shift+Right".action = move-column-right;
      "super+Shift+Down".action = move-column-to-workspace-down;
      "super+Shift+Up".action = move-column-to-workspace-up;

      "super+1".action = focus-workspace "1";
      "super+2".action = focus-workspace "2";
      "super+3".action = focus-workspace "3";
      "super+4".action = focus-workspace "4";
      "super+5".action = focus-workspace "5";
    };
}
