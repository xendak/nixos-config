{
  inputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    inputs.hyprland.homeManagerModules.default
    ./scripts.nix
  ];

  programs = {
    fish.loginShellInit = ''
      if test (tty) = "/dev/tty1"
        exec Hyprland &> /dev/null
      end
    '';
  };

  xdg.portal = let
    hyprland = config.wayland.windowManager.hyprland.package;
    xdph = pkgs.xdg-desktop-portal-hyprland.override {inherit hyprland;};
  in {
    extraPortals = [xdph];
    configPackages = [hyprland];
  };

  home.packages = [
    pkgs.swayidle
    pkgs.brightnessctl
    inputs.hyprwm-contrib.packages.${pkgs.system}.grimblast
    inputs.hyprpicker.packages.${pkgs.system}.hyprpicker
    # inputs.hyprland-portal.packages.${pkgs.system}.xdg-desktop-portal-hyprland
    # inputs.hyprland-portal.packages.${pkgs.system}.hyprland-share-picker
  ];

  # programs.waybar.package = pkgs.waybar.overrideAttrs (oa: {
  #   mesonFlags = (oa.mesonFlags or [ ]) ++ [ "-Dexperimental=true" ];
  # });

  wayland.windowManager.hyprland = {
    enable = true;

    # package = pkgs.hyprland.override {wrapRuntimeDeps = false;};
    systemd = {
      enable = true;
      # Same as default, but stop graphical-session too
      extraCommands = lib.mkBefore [
        "systemctl --user stop graphical-session.target"
        "systemctl --user start hyprland-session.target"
      ];
    };


    settings = let
      rofi = "${pkgs.rofi}/bin/rofi";
      # grimblast = "${pkgs.inputs.hyprwm-contrib.grimblast}/bin/grimblast";
      grimblast = "grimblast";
      wpctl = "${pkgs.wireplumber}/bin/wpctl";
      brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl";
      # swaylock = "${config.programs.swaylock.package}/bin/swaylock";
      playerctl = "${config.services.playerctld.package}/bin/playerctl";
      c = config.colorscheme.palette;
      wallpaper =
        if config.home.username == "drops"
        then "$HOME/Flake/home/common/wallpapers/10.jpg"
        else "$HOME/Flake/home/common/wallpapers/13.jpg";
    in {
      exec-once =
        [
          "swww-daemon"
          "swww img ${wallpaper}"
          "ags -c $HOME/Flake/home/${config.home.username}/ags/config.js"
          "mkdir -p $HOME/tmp/Screenshots"
          "hyprctl setcursor '${config.gtk.cursorTheme.name}' 32"
          "swayidle -w"
        ]
        ++ (
          if config.home.username == "flakes"
          then [
            "sh $HOME/Flake/bin/bt-once.sh"
            "openrgb -d \"XPG Spectrix S40G\" -m Off"
          ]
          else []
        );

      debug = {
        disable_logs = false;
      };

      monitor = map (
        m: let
          resolution = "${toString m.width}x${toString m.height}@${toString m.refreshRate}";
          position = "${toString m.x}x${toString m.y}";
        in "${m.name},${
          if m.enabled
          then "${resolution},${position},1"
          else "disable"
        }"
      ) (config.monitors);

      workspace =
        [
          "3, maximize"
          "4, float"
        ]
        ++ map (
          m: "${m.name},${m.workspace}"
        ) (lib.filter (m: m.enabled && m.workspace != null) config.monitors);

      general = {
        gaps_in = 5;
        gaps_out = 20;
        border_size = 3;
        "col.active_border" = "rgba(${c.base0C}ff)";
        "col.inactive_border" = "rgba(${c.base02}a0)";
        resize_on_border = true;
        hover_icon_on_border = false;
      };

      decoration = {
        active_opacity = 0.99;
        inactive_opacity = 0.89;
        fullscreen_opacity = 1.0;
        rounding = 9;

        shadow = {
          enabled = true;
          range = 30;
          color = "rgba(${c.base0C}f0)";
          color_inactive = "rgba(${c.base00}66)";
        };

        blur = {
          enabled = true;
          size = 8;
          passes = 3;
          new_optimizations = "on";
          noise = 0.01;
          contrast = 0.9;
          brightness = 0.8;
          popups = true;
        };
      };

      animations = {
        enabled = true;

        bezier = [
          "easein,0.11, 0, 0.5, 0"
          "easeout,0.5, 1, 0.89, 1"
          "easeinout,0.45, 0, 0.55, 1"
          "whoa,0.68,0.25,0.265,1.25"
          "smooth,0.445,0.05,0.55,0.95"
          "slow,0,0.85,0.3,1"
          "overshot,0.7,0.6,0.1,1.1"
        ];

        animation = [
          "windowsIn,1,3,easeout,slide"
          "windowsOut,1,3,easein,slide"
          "windowsMove,1,0.5,easeout"
          "fadeIn,1,3,easeout"
          "fadeOut,1,3,easein"
          "fadeSwitch,1,3,easeout"
          "fadeShadow,1,3,easeout"
          "fadeDim,1,3,easeout"
          "border,1,3,easeout"
          "workspaces,1,2,easeout,slide"
        ];
      };

      misc = {
        vfr = true;
        vrr = 1;
        mouse_move_enables_dpms = true;
      };

      gestures = {
        workspace_swipe = true;
      };

      cursor = {
        no_warps = true;
      };

      input = {
        kb_layout = "us";
        kb_variant = "altgr-intl";
        kb_options = "ctrl:nocaps";
        kb_rules = "evdev";
        follow_mouse = 2;
        float_switch_override_focus = 2;
        touchpad = {
          disable_while_typing = false;
          natural_scroll = 1;
        };
      };

      dwindle = {
        split_width_multiplier = 1.35;
        pseudotile = true;
      };

      binds = {
        allow_workspace_cycles = true;
      };

      layerrule = [
        "blur, ^(gtk-layer-shell)$"
        "ignorezero, ^(gtk-layer-shell)$"
      ];

      windowrulev2 = let
        float = regex: "float, class:^(${regex})$";
        t_float = regex: "float, title:^(${regex})$";
        t_size = regex: "size 1300 800, title:^(${regex})$";
        size = regex: "size 1300 800, class:^(${regex})$";
        minsize = regex: "minsize 1300 800, title:^(${regex})$";
        maximize = regex: "maximize, title:^(${regex})$";
        f_input = regex: "forceinput, title:^(${regex})$";
        outoftheway = regex: "workspace 9 silent, title:^(${regex})$";
        ly = regex: "${regex}, class:^(io.github.waylyrics.Waylyrics)$";
      in [
        (float "(.*)(league)(.*)")
        (float "(org.gnome)(.*)")
        (float "(org.kde)(.*)")
        (float "pavucontrol")
        (float "f_terminal")
        (float "moe.laucher(.*)")
        (float "mpv")
        (float "Winetricks")
        (float "Picture-in-(.*)")
        (float "deluge")
        (float "rustdesk")
        (maximize "(.*)@toast(.*)")
        # (f_input "Terraria")
        # (f_input "gamescope")
        # (f_input "(.*)(.exe)")
        (maximize "Terraria")
        (maximize "gamescope")
        (minsize "(.*)(.exe)")
        (size "f_terminal")
        (t_size "(.*)(Home)(.*)")
        (t_size "(.*)(Save)(.*)")
        (ly "float")
        (ly "size 100% 10%")
        (ly "move 0 0")
        (ly "pin")
        (ly "nofocus")
        (ly "noblur")
        (ly "nodim")
        (ly "noanim")
        (ly "noshadow")
        (ly "noborder")
        "workspace special silent, title:^((.*)@toast(.*))$"
        "workspace 5 silent, class:^(rustdesk)$"
        "workspace special silent, title:^((.*)@toast(.*))$"
        "workspace 2 silent, class:^(firefox)$"
        "workspace 3 silent, class:^(discord)(.*)$"
        "workspace 3 silent, class:^(vesktop)(.*)$"
        # All Steam Rules
        "float, initialClass:steam"
        "float, initialClass:((.*)steam(.*))$"
        "workspace 4 silent, initialTitle:^(Steam)$,initialClass:^(steam)$"
        "move 10 10%, initialTitle:^(Steam)$,initialClass:^(steam)$"
        "size 80% 90%, initialTitle:^(Steam)$,initialClass:^(steam)$"
        "workspace 4 silent, initialTitle:^(Friends(.*))$,initialClass:^(steam)$"
        "move 84% 10%, initialTitle:^(Friends(.*))$,initialClass:^(steam)$"
        "size 14% 90%, initialTitle:^(Friends(.*))$,initialClass:^(steam)$"
        "workspace 4 silent, initialTitle:^((.*)Browser(.*))$,initialClass:^(steam)$"
        "move 20% 30%, initialTitle:^((.*)Browser(.*))$,initialClass:^(steam)$"
        "size 70% 70%, initialTitle:^((.*)Browser(.*))$,initialClass:^(steam)$"

        "workspace 4 silent, class:^(lutris)$"
        (outoftheway "(.*)(Sharing)(.*)")
      ];

      bindm = [
        "SUPER, mouse:272, movewindow"
        "SUPERSHIFT, mouse:272, resizewindow"
        "SUPER, mouse:273, resizewindow"
      ];

      bind = let
        binding = mod: cmd: key: arg: "${mod}, ${key}, ${cmd}, ${arg}";
        mvfocus = binding "SUPER" "movefocus";
        ws = binding "SUPER" "workspace";
        resizeactive = binding "SUPER CTRL" "resizeactive";
        mvactive = binding "SUPER ALT" "moveactive";
        mvtows = binding "SUPER SHIFT" "movetoworkspace";
        focusmon = binding "SUPER CONTROL" "focusmonitor";
        wintomon = binding "SUPER CONTROL SHIFT" "movewindow";
        worktomon = binding "SUPER ALT" "movecurrentworkspacetomonitor";
        e = "exec, ags";
        arr = [1 2 3 4 5 6 7 8 9];
        terminal = config.home.sessionVariables.TERMINAL;
        browser = config.home.sessionVariables.BROWSER;
        # editor = config.home.sessionVariables.EDITOR;
        filebrowser = config.home.sessionVariables.FILEBROWSER;
        termbrowser = config.home.sessionVariables.TERMBROWSER;
        print = "$HOME/Pictures/Screenshots/$(date +%Y-%m-%d-%M)";
        tmpprint = "$HOME/tmp/Screenshots/$(date +%Y-%m-%d-%M-%S)";
      in
        [
          "SUPER, Return,       exec,     ${terminal}"
          "SUPERSHIFT, Return,  exec,     ${terminal} --class f_terminal"
          "SUPER, W,            exec,     ${browser}"
          "SUPER, E,            exec,     ${filebrowser}"
          "SUPERSHIFT, E,       exec,     ${terminal} --class f_terminal -e $SHELL -ic '${termbrowser} -ndeiH'"
          "ALTSHIFT,   E,       exec,     ${terminal} -e $SHELL -ic '${termbrowser} -ndeiH'"
          "SUPER, D,            exec,     rofi -show drun -matching fuzzy -sorting-method fzf -sort -drun-match-fields name,generic,categories -theme \"${config.xdg.configHome}/rofi/config.rasi\""
          "SUPER, R,            ${e} -t applauncher"
          "SUPER, O,            ${e} -t overview"
          "SUPER, M,            ${e} -t datemenu"
          "SUPER, B,            ${e} -t quicksettings"
          ",XF86PowerOff,       ${e} -r 'powermenu.shutdown()'"
          # ",XF86Launch4,   ${e} -r 'recorder.start()'"
          # ",Print,         ${e} -r 'recorder.screenshot()'"
          # "SHIFT,Print,    ${e} -r 'recorder.screenshot(true)'"

          "SUPER, Tab, focuscurrentorlast"
          "SUPER, Q, killactive"
          "SUPERSHIFT, space, togglefloating"
          "SUPERSHIFT, F, fullscreen"
          "SUPER, F, fullscreen, 1"
          "SUPER, P, togglesplit"
          "ALT, Tab, togglespecialworkspace"
          "ALTSHIFT, tab, movetoworkspace, special"

          ",Print, exec, ${grimblast} --notify copysave output \"${print}_full.png\""
          "SHIFT,Print, exec, ${grimblast} --notify copysave active \"${print}_active.png\""
          "ALTSHIFT,S, exec, ${grimblast} --notify copysave area \"${print}_snip.png\""
          "ALTSHIFT,C, exec, ${grimblast} --notify copysave area \"${tmpprint}_snip.png\""

          (mvfocus "k" "u")
          (mvfocus "j" "d")
          (mvfocus "l" "r")
          (mvfocus "h" "l")
          (mvtows "left" "e-1")
          (mvtows "right" "e+1")
          (resizeactive "k" "0 -20")
          (resizeactive "j" "0 20")
          (resizeactive "l" "20 0")
          (resizeactive "h" "-20 0")
          (mvactive "k" "0 -20")
          (mvactive "j" "0 20")
          (mvactive "l" "20 0")
          (mvactive "h" "-20 0")

          # with arrow keys
          (mvfocus "up" "u")
          (mvfocus "down" "d")
          (mvfocus "right" "r")
          (mvfocus "left" "l")
          (resizeactive "up" "0 -20")
          (resizeactive "down" "0 20")
          (resizeactive "right" "20 0")
          (resizeactive "left" "-20 0")
          (mvactive "up" "0 -20")
          (mvactive "down" "0 20")
          (mvactive "right" "20 0")
          (mvactive "left" "-20 0")

          # monitor kbs
          (focusmon "k" "u")
          (focusmon "j" "d")
          (focusmon "l" "r")
          (focusmon "h" "l")
          (worktomon "k" "u")
          (worktomon "j" "d")
          (worktomon "l" "r")
          (worktomon "h" "l")
          (wintomon "k" "mon:u")
          (wintomon "j" "mon:d")
          (wintomon "l" "mon:r")
          (wintomon "h" "mon:l")

          (focusmon "up" "u")
          (focusmon "down" "d")
          (focusmon "right" "r")
          (focusmon "left" "l")
          (worktomon "up" "u")
          (worktomon "down" "d")
          (worktomon "right" "r")
          (worktomon "left" "l")
          (wintomon "up" "mon:u")
          (wintomon "down" "mon:d")
          (wintomon "right" "mon:r")
          (wintomon "left" "mon:l")
        ]
        ++ (map (i: ws (toString i) (toString i)) arr)
        ++ (map (i: mvtows (toString i) (toString i)) arr);

      bindle = [
        "SUPER, bracketright,   exec, ${brightnessctl} set +5%"
        "SUPER, bracketleft,    exec, ${brightnessctl} set  5%-"
        ",XF86AudioRaiseVolume, exec, ${wpctl} set-volume @DEFAULT_SINK@ 0.05+"
        ",XF86AudioLowerVolume, exec, ${wpctl} set-volume @DEFAULT_SINK@ 0.05-"
        "SUPERSHIFT,period,exec,${wpctl} set-volume @DEFAULT_SINK@ 0.05+"
        "SUPERSHIFT,comma,exec,${wpctl} set-volume @DEFAULT_SINK@ 0.05-"
        "SUPERSHIFT,slash,exec,${wpctl} set-mute @DEFAULT_SINK@ toggle"
        ",XF86AudioMicMute, exec, ${wpctl} set-mute @DEFAULT_SOURCE@ toggle"
      ];

      bindl = [
        ",XF86AudioPlay,    exec, ${playerctl} play-pause"
        ",XF86AudioStop,    exec, ${playerctl} pause"
        ",XF86AudioPause,   exec, ${playerctl} pause"
        ",XF86AudioPrev,    exec, ${playerctl} previous"
        ",XF86AudioNext,    exec, ${playerctl} next"
      ];
    };

    # OPEN RGB FOR DESKTOP and Custom commands
    extraConfig = ''
      env = bitdepth,10
      # lyrics & ocr submap since most keys arent being recognized
      # TODO: make a script to visualize this mode
      # bind=SUPER,X,exec,sh "${config.xdg.configHome}/rofi/powermenu.sh"
      bind=ALTSHIFT,L,submap,lyrics
      submap=lyrics
      binde=,s,exec, wl-ocr
      binde=,l,exec, waylyrics
      binde=,q,exec, pkill waylyrics
      binde=,f,exec, wl-waylyrics
      bind=,escape, submap,reset
      submap=reset

      # powermenu submap
      bind=SUPER,X,exec,sh "${config.xdg.configHome}/rofi/powermenu.sh"
      bind=SUPER,X,submap,powermenu
      submap=powermenu
      binde=,r,exec,systemctl reboot
      binde=,d,exec,systemctl poweroff
      binde=,q,exec,systemctl pkill Hyprland
      binde=,p,exec,systemctl mpc -q pause && wpctl set-mute @DEFAULT_SINK@ toggle && systemctl suspend
      binde=,escape,exec,pkill rofi

      bind=,escape,submap,reset
      submap=reset
    '';

    # extraConfig =
    #   (import ./monitors.nix {
    #     inherit lib;
    #     inherit (config) monitors;
    #   }) +
    #   # +
    #   # builtins.replaceStrings ["#TRANSFORM"]
    #   # [
    #   # 	( "monitor=DP-2,transform,3" )
    #   # ]
    #   (import ./${config.home.username}-config.nix {
    #     inherit (config) colorscheme;
    #     inherit config;
    #   });
  };
}
