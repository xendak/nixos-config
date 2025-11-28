{
  lib,
  config,
  pkgs,
  ...
}:
let
  # TODO: hard code this till niri accepts imports in its files.
  colorscheme = import ../../colors/palettes/gorgoroth.nix;
  c = colorscheme.palette;

  generateNiriOutput = monitor: {
    name = monitor.name;
    value = {
      mode = {
        width = monitor.width;
        height = monitor.height;
        refresh = monitor.refreshRate;
      };
      position = {
        x = monitor.x;
        y = monitor.y;
      };
      scale = 1.0;
      variable-refresh-rate = true;
    };
  };
in
{
  home.packages = [
    pkgs.xwayland-satellite
    pkgs.swaylock
    pkgs.hyprpicker
  ];

  programs.niri = {
    enable = true;
    package = pkgs.niri;
    settings = {
      workspaces = {
        "1" = { };
        "2" = { };
        "3" = { };
        "4" = { };
        "5" = { };
      };

      prefer-no-csd = true;
      screenshot-path = "/home/${config.home.username}/Pictures/Screenshots/%Y-%m-%d-%H-%M-%S_window.png";

      hotkey-overlay = {
        skip-at-startup = true;
      };

      overview = {
        backdrop-color = "transparent";
        workspace-shadow.enable = false;
      };

      layout = {
        background-color = "transparent";

        insert-hint = {
          enable = true;
          # display.color = "${c.wm_active_border}30";
          display.gradient = {
            from = "${c.wm_active_border}50";
            to = "#ff8c0060";
            angle = 135;
            in' = "oklch shorter hue";
          };
        };

        border = {
          width = 1;
          active.color = c.wm_inactive_border;
          inactive.color = c.wm_inactive_border;
        };

        focus-ring = {
          width = 1;
          active.gradient = {
            from = "${c.wm_active_border}50";
            to = "${c.wm_inactive_border}50";
            angle = 135;
            in' = "oklch longer hue";
          };
        };

        tab-indicator = {
          hide-when-single-tab = true;
          place-within-column = true;
          position = "left";
          gaps-between-tabs = 12;
          width = 8;
          gap = -12;
          length = {
            total-proportion = 0.5;
          };
          corner-radius = 12;
          active.gradient = {
            from = "${c.wm_active_border}";
            to = "${c.wm_inactive_border}";
            angle = 45;
            in' = "oklch shorter hue";
          };
          inactive.color = "${c.base02}";
        };

        preset-column-widths = [
          { proportion = 0.33333; }
          { proportion = 0.5; }
          { proportion = 0.66667; }
          { proportion = 1.0; }
        ];

        preset-window-heights = [
          { proportion = 0.5; }
          { proportion = 1.0; }
        ];
        always-center-single-column = true;
        center-focused-column = "never";
        default-column-display = "normal";

        default-column-width = {
          proportion = 0.5;
        };

        shadow = {
          enable = true;
          softness = 15;
          spread = 1;
          offset = {
            x = 0;
            y = 0;
          };
          draw-behind-window = false;
          color = c.wm_active_border;
          inactive-color = c.wm_inactive_border;
        };

        gaps = 20;

        struts = {
          left = -5;
          right = -5;
          top = -5;
          bottom = -5;
        };
      };

      input = {
        workspace-auto-back-and-forth = true;

        tablet = {
          left-handed = true;
        };

        keyboard = {
          xkb = {
            layout = "us";
            variant = "altgr-intl";
            options = "ctrl:nocaps";
            rules = "evdev";
          };
          repeat-delay = 200;
          repeat-rate = 15;
        };

        touchpad = {
          click-method = "button-areas";
          dwt = true;
          dwtp = true;
          natural-scroll = true;
          scroll-method = "two-finger";
          tap = true;
          tap-button-map = "left-right-middle";
          middle-emulation = true;
          accel-profile = "adaptive";
        };
        focus-follows-mouse = {
          enable = true;
          max-scroll-amount = "0%";
        };
        warp-mouse-to-focus.enable = false;
      };

      outputs = lib.listToAttrs (map generateNiriOutput config.monitors);
      cursor = {
        size = config.gtk.cursorTheme.size;
        theme = config.gtk.cursorTheme.name;
      };

      # animations.window-open = {
      #   custom-shader = builtins.readFile ./shaders/window-open.glsl;
      #   kind.easing = {
      #     curve = "ease-out-cubic";
      #     duration-ms = 200;
      #   };
      # };
      # animations.window-close = {
      #   custom-shader = builtins.readFile ./shaders/window-close.glsl;
      #   kind.easing = {
      #     curve = "ease-out-quad";
      #     duration-ms = 250;
      #   };
      # };
      animations.window-resize = {
        custom-shader = builtins.readFile ./shaders/window-resize.glsl;
        kind.easing = {
          curve = "ease-out-cubic";
          duration-ms = 200;
        };
      };

      environment = {
        NIX_AUTO_RUN = "1";
        MOZ_ENABLE_WAYLAND = "1";
        ANKI_WAYLAND = "1";
        NIXOS_OZONE_WL = "1";
        LIBSEAT_BACKEND = "logind";
        QT_QPA_PLATFORM = "wayland;xcb";
        # tthis seems to work better overall?
        QT_QPA_PLATFORMTHEME = "kde";
        # QT_QPA_PLATFORMTHEME = "qt5ct";
        QT_WAYLAND_DISABLE_WINDOWDECORATION = "0";
        WLR_NO_HARDWARE_CURSORS = "1";
        INPUT_METHOD = "fcitx";
        XIM_SERVERS = "fcitx";
        XMODIFIERS = "@im=fcitx";
        XMODIFIER = "@im=fcitx";
        GTK_IM_MODULE = "fcitx";
        QT_IM_MODULE = "fcitx";
        QT_IM_MODULES = "wayland;fcitx;ibus";
        CLUTTER_BACKEND = "wayland";
        GDK_BACKEND = "wayland,x11";
        ELECTRON_OZONE_PLATFORM_HINT = "auto";
        ELECTRON_ENABLE_HARDWARE_ACCELERATION = "1";
        XDG_SESSION_TYPE = "wayland";
        XDG_CURRENT_DESKTOP = "niri";
        DISPLAY = ":0";
      };
    };
  };
}
