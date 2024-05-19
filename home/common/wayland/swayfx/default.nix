{
  config,
  pkgs,
  inputs,
  lib,
  ...
}: let
  colors = config.colorscheme.palette;
  wallpaper =
    if config.home.username == "drops"
    then "$HOME/Flake/home/common/wallpapers/10.jpg"
    else "$HOME/Flake/home/common/wallpapers/13.jpg";
  swayfx-unwrapped = (pkgs.swayfx-unwrapped.override {wlroots = pkgs.wlroots_0_16;}).overrideAttrs (old: {
    nativeBuildInputs = old.nativeBuildInputs ++ [pkgs.cmake];
  });

  # Function to generate sway output configuration from monitor settings
  generateSwayOutputConfig = monitor: {
    resolution = "${toString monitor.width}x${toString monitor.height}";
    position = "${toString monitor.x},${toString monitor.y}";
    refreshRate = toString monitor.refreshRate;
  };

  # Create the outputs configuration for all monitors
  outputs = lib.lists.foldl' (cfg: monitor:
    cfg // { "${monitor.name}" = generateSwayOutputConfig monitor; }
  ) {} config.monitors;

in {
  programs = {
    fish.loginShellInit = ''
      if test (tty) = "/dev/tty1"
        exec sway &> /dev/null
      end
    '';
  };

  wayland.windowManager.sway = {
    enable = true;
    systemd.enable = true;
    xwayland = true;
    checkConfig = false;
    package = inputs.swayfx.packages.${pkgs.system}.default;
    extraConfig = ''
      ## SWAYFX CONFIG
      corner_radius 14
      shadows on
      shadow_offset 0 0
      shadow_blur_radius 20
      shadow_color #${colors.base09}BB
      shadow_inactive_color #000000B0

      default_dim_inactive 0.2

      layer_effects "notif" blur enable; shadows enable; corner_radius 20
      layer_effects "osd" blur enable; shadows enable; corner_radius 20
      layer_effects "work" shadows enable
      layer_effects "panel" shadows enable
      layer_effects "calendarbox" shadows enable; corner_radius 12

      for_window [app_id="spad"] move scratchpad, resize set width 900 height 600
      for_window [app_id="smusicpad"] move scratchpad, resize set width 850 height 550

      set $bg-color           #${colors.base00}
      set $inactive-bg-color  #${colors.base02}
      set $text-color         #${colors.base05}
      set $inactive-text-color#${colors.base04}
      set $urgent-bg-color    #${colors.base09}

      # window colors
      #                       border              background         text                 indicator
      client.focused          $bg-color           $bg-color          $text-color          $bg-color
      client.unfocused        $inactive-bg-color  $inactive-bg-color $inactive-text-color $inactive-bg-color
      client.focused_inactive $inactive-bg-color  $inactive-bg-color $inactive-text-color $inactive-bg-color
      client.urgent           $urgent-bg-color    $urgent-bg-color   $text-color          $urgent-bg-color

      font pango:Sofia Pro 12
      titlebar_separator enable
      titlebar_padding 16
      title_align center
      default_border normal 2
      default_floating_border normal 2
    '';
    config = {
      terminal = "kitty";
      menu = "rofi -show drun -matching fuzzy -sorting-method fzf -sort -theme \"${config.xdg.configHome}/rofi/config.rasi\"";
      modifier = "Mod4";

      keycodebindings = let
        cfg = config.wayland.windowManager.sway.config;
        mod = cfg.modifier;
        left = "43"; # h
        down = "44"; # j
        up = "45"; # k
        right = "46"; # l
      in {
        "${mod}+${left}" = "focus left";
        "${mod}+${down}" = "focus down";
        "${mod}+${up}" = "focus up";
        "${mod}+${right}" = "focus right";

        "${mod}+Shift+${left}" = "move left";
        "${mod}+Shift+${down}" = "move down";
        "${mod}+Shift+${up}" = "move up";
        "${mod}+Shift+${right}" = "move right";
      };

      keybindings = let
        cfg = config.wayland.windowManager.sway.config;
        mod = cfg.modifier;
        print = "$HOME/Pictures/Screenshots/$(date +%Y-%m-%d-%M)";
        tmpprint = "$HOME/Games/tmp/Screenshots/$(date +%Y-%m-%d-%M-%S)";
        wpctl = "${pkgs.wireplumber}/bin/wpctl";
        brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl";
        playerctl = "${config.services.playerctld.package}/bin/playerctl";
        termbrowser = config.home.sessionVariables.TERMBROWSER;
        filebrowser = config.home.sessionVariables.FILEBROWSER;
        browser = config.home.sessionVariables.BROWSER;
        terminal = config.home.sessionVariables.TERMINAL;
      in {
        "print" = "exec 'grimblast --notify copysave output \"${print}_full.png\"'";
        "Shift+Print" = "exec 'grimblast --notify copysave active \"${print}_active.png\"'";
        "Alt+Shift+s" = "exec 'grimblast --notify copysave area \"${print}_snip.png\"'";
        "Alt+Shift+c" = "exec 'grimblast --notify copysave area \"${tmpprint}_snip.png\"'";

        "XF86MonBrightnessUp" = "exec 'brightnessctl s 5+'";
        "XF86MonBrightnessDown" = "exec 'brightnessctl s 5-'";
        "${mod}+bracketright" = "exec '${brightnessctl} set +5%'";
        "${mod}+bracketleft" = "exec '${brightnessctl} set 5%-'";
        "XF86AudioRaiseVolume" = "exec '${wpctl} set-volume @DEFAULT_SINK@ 0.05+'";
        "XF86AudioLowerVolume" = "exec '${wpctl} set-volume @DEFAULT_SINK@ 0.05-'";
        "${mod}+Shift+period" = "exec '${wpctl} set-volume @DEFAULT_SINK@ 0.05+'";
        "${mod}+Shift+comma" = "exec '${wpctl} set-volume @DEFAULT_SINK@ 0.05-'";
        "${mod}+Shift+slash" = "exec '${wpctl} set-mute @DEFAULT_SINK@ toggle'";
        "XF86AudioMicMute" = "exec '${wpctl} set-mute @DEFAULT_SOURCE@ toggle'";
        "XF86AudioPlay" = "exec '${playerctl} play-pause'";
        "XF86AudioStop" = "exec '${playerctl} pause'";
        "XF86AudioPause" = "exec '${playerctl} pause'";
        "XF86AudioPrev" = "exec '${playerctl} previous'";
        "XF86AudioNext" = "exec '${playerctl} next'";

        "${mod}+Return" = "exec ${terminal}";
        "${mod}+Shift+r" = "reload";
        "${mod}+d" = "exec ${cfg.menu}";
        "${mod}+Shift+e" = "exec ${cfg.terminal} --class f_terminal -e $SHELL -ic '${termbrowser} -ndeiH'";
        "Alt+Shift+e" = "exec ${cfg.terminal} -e $SHELL -ic '${termbrowser} -ndeiH'";
        "${mod}+e" = "exec ${filebrowser}";
        "${mod}+w" = "exec ${browser}";

        "${mod}+v" = "exec 'swayscratch spad'";
        "${mod}+z" = "exec 'swayscratch smusicpad'";

        "${mod}+Left" = "focus left";
        "${mod}+Down" = "focus down";
        "${mod}+Up" = "focus up";
        "${mod}+Right" = "focus right";

        "${mod}+Shift+Left" = "move left";
        "${mod}+Shift+Down" = "move down";
        "${mod}+Shift+Up" = "move up";
        "${mod}+Shift+Right" = "move right";

        "${mod}+Alt+b" = "splith";
        "${mod}+Shift+v" = "splitv";
        "${mod}+f" = "fullscreen";
        "${mod}+a" = "focus parent";

        "${mod}+s" = "layout stacking";
        "${mod}+Shift+s" = "layout tabbed";

        "${mod}+p" = "layout toggle split";

        "${mod}+space" = "floating toggle";
        "${mod}+Shift+space" = "focus mode_toggle";

        "${mod}+1" = "workspace number 1";
        "${mod}+2" = "workspace number 2";
        "${mod}+3" = "workspace number 3";
        "${mod}+4" = "workspace number 4";
        "${mod}+5" = "workspace number 5";
        "${mod}+6" = "workspace number 6";
        "${mod}+7" = "workspace number 7";
        "${mod}+8" = "workspace number 8";
        "${mod}+9" = "workspace number 9";
        "${mod}+0" = "workspace number 10";

        "${mod}+Shift+1" = "move container to workspace number 1";
        "${mod}+Shift+2" = "move container to workspace number 2";
        "${mod}+Shift+3" = "move container to workspace number 3";
        "${mod}+Shift+4" = "move container to workspace number 4";
        "${mod}+Shift+5" = "move container to workspace number 5";
        "${mod}+Shift+6" = "move container to workspace number 6";
        "${mod}+Shift+7" = "move container to workspace number 7";
        "${mod}+Shift+8" = "move container to workspace number 8";
        "${mod}+Shift+9" = "move container to workspace number 9";
        "${mod}+Shift+0" = "move container to workspace number 10";

        "${mod}+Shift+minus" = "move scratchpad";
        "${mod}+minus" = "scratchpad show";

        "${mod}+q" = "kill";
        "${mod}+r" = "mode resize";
      };
      input = {
        "type:touchpad" = {
          tap = "enabled";
          natural_scroll = "enabled";
        };
        "*" = {
          xkb_layout = "us";
          xkb_variant = "altgr-intl";
          xkb_options = "ctrl:nocaps";
        };
      };
      output = outputs;

      startup = [
        { command = "swww init && swww img ${wallpaper}"; }
        { command = "ags -c $HOME/Flake/home/${config.home.username}/ags/config.js"; }
        { command = "mkdir -p $HOME/tmp"; }
        { command = "swayidle -w"; }
        { command = "openrgb -d \"XPG Spectrix S40G\" -m Off"; }
      ];

      bars = [];
      gaps = {
        bottom = 5;
        horizontal = 5;
        vertical = 5;
        inner = 5;
        left = 5;
        outer = 5;
        right = 5;
        top = 5;
        smartBorders = "off";
        smartGaps = false;
      };
    };
  };
}

