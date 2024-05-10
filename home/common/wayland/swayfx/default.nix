{
  config,
  pkgs,
  inputs,
  ...
}: let
  colors = config.colorscheme.palette;
  swayfx-unwrapped = (pkgs.swayfx-unwrapped.override {wlroots = pkgs.wlroots_0_16;}).overrideAttrs (old: {
    #version = "0.4.0-git";
    #src = pkgs.lib.cleanSource inputs.swayfx;
    nativeBuildInputs = old.nativeBuildInputs ++ [pkgs.cmake];
    #buildInputs = old.buildInputs ++ [ pkgs.scenefx pkgs.mesa pkgs.libdrm ];
  });
  # swayfx-unwrapped = inputs.swayfx.packages.${pkgs.system}.default;
in {
  programs = {
    fish.loginShellInit = ''
      if test (tty) = "/dev/tty1"
        exec sway &> /dev/null
      end
    '';
  };

  # systemd.user.targets.hyprland-session.Unit.Wants = [ "xdg-desktop-autostart.target" ];
  wayland.windowManager.sway = {
    enable = true;
    systemd.enable = true;
    xwayland = true;
    checkConfig = false;
    #package = inputs.swayfx.packages.${pkgs.system}.default;
    # package = pkgs.swayfx.override {inherit swayfx-unwrapped;};
    #package = inputs.swayfx.packages.${pkgs.system}.swayfx-unwrapped;
    # package = pkgs.swayfx.override {inherit swayfx-unwrapped;};
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
      layer_effects "work"  shadows enable
      layer_effects "panel" shadows enable
      layer_effects "calendarbox"shadows enable; corner_radius 12

      for_window [app_id="spad"] move scratchpad, resize set width 900 height 600
      for_window [app_id="smusicpad"] move scratchpad, resize set width 850 height 550

      set $bg-color 	         #${colors.base00}
      set $inactive-bg-color   #${colors.base02}
      set $text-color          #${colors.base05}
      set $inactive-text-color #${colors.base04}
      set $urgent-bg-color     #${colors.base09}

      # window colors
      #                       border              background         text                 indicator
      client.focused          $bg-color           $bg-color          $text-color          $bg-color
      client.unfocused        $inactive-bg-color $inactive-bg-color $inactive-text-color  $inactive-bg-color
      client.focused_inactive $inactive-bg-color $inactive-bg-color $inactive-text-color  $inactive-bg-color
      client.urgent           $urgent-bg-color    $urgent-bg-color   $text-color          $urgent-bg-color

      font pango:Sofia Pro 12
      titlebar_separator enable
      titlebar_padding 16
      title_align center
      default_border normal 2
      default_floating_border normal 2

      exec_always --no-startup-id xrdb -merge ~/.Xresources &
      exec --no-startup-id ags &
      exec_always --no-startup-id mpDris2 &
      exec_always --no-startup-id autotiling-rs &
      exec --no-startup-id swayidle -w \
          timeout 360 'waylock' \
          timeout 600 'swaymsg "output * power off"' resume 'swaymsg "output * power on"' \
          before-sleep 'waylock'
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
      in {
        "print" = "exec 'grim -g \"$(slurp)\" - | wl-copy'";
        "Shift+print" = "exec 'grim - | wl-copy'";

        "XF86MonBrightnessUp" = "exec 'brightnessctl s 5+'";
        "XF86MonBrightnessDown" = "exec 'brightnessctl s 5-'";

        "XF86AudioRaiseVolume" = "exec 'pamixer -u ; pamixer -i 5'";
        "XF86AudioLowerVolume" = "exec 'pamixer -u ; pamixer -d 5'";
        "XF86AudioMute" = "exec 'pamixer -t'";

        "${mod}+Return" = "exec ${cfg.terminal}";
        "${mod}+Shift+q" = "reload";
        "${mod}+Shift+b" = "exec 'pkill ags ; ags & disown'";
        "${mod}+d" = "exec ${cfg.menu}";
        "${mod}+e" = "exec dolphin";
        "${mod}+w" = "exec firefox";

        "${mod}+v" = "exec 'swayscratch spad'";
        "${mod}+z" = "exec 'swayscratch smusicpad'";
        #"${mod}+${cfg.left}" = "focus left";
        #"${mod}+${cfg.down}" = "focus down";
        #"${mod}+${cfg.up}" = "focus up";
        #"${mod}+${cfg.right}" = "focus right";

        "${mod}+Left" = "focus left";
        "${mod}+Down" = "focus down";
        "${mod}+Up" = "focus up";
        "${mod}+Right" = "focus right";

        #"${mod}+Shift+${cfg.left}" = "move left";
        #"${mod}+Shift+${cfg.down}" = "move down";
        #"${mod}+Shift+${cfg.up}" = "move up";
        #"${mod}+Shift+${cfg.right}" = "move right";

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

        "${mod}+shift+e" = "layout toggle split";

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

        "${mod}+Shift+c" = "kill";
        "${mod}+r" = "mode resize";
      };
      input = {
        "type:touchpad" = {
          tap = "enabled";
          natural_scroll = "enabled";
        };
        "*" = {
          xkb_layout = "algr-intl";
          xkb_options = "ctrl:nocaps";
        };
      };
      output = {
        "eDP-1" = {
          resolution = "1920x1080";
          position = "0,0";
        };
      };
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
