{
  lib,
  config,
  pkgs,
  ...
}:
let
  c = config.colorscheme.palette;
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
      # You can add other options here
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
      screenshot-path = "${config.home.username}/Pictures/Screenshots/%Y-%m-%d-%H-%M-%S_window.png";

      hotkey-overlay = {
        skip-at-startup = true;
      };

      overview = {
        backdrop-color = "transparent";
        workspace-shadow.enable = false;
      };

      layout = {
        background-color = "transparent";

        # focus-ring = {
        #   enable = true;
        #   width = 2;
        #   active = {
        #     color = c.base10;
        #   };
        #   inactive = {
        #     color = c.base11;
        #   };
        # };
        insert-hint = {
          enable = true;
          # display.color = "#${c.base10}30";
          display.gradient = {
            from = "#${c.base10}50";
            to = "#ff8c0060";
            angle = 135;
            in' = "oklch shorter hue";
          };
        };

        border = {
          width = 1;
          active.color = c.base11;
          inactive.color = c.base11;
        };

        focus-ring = {
          width = 1;
          active.gradient = {
            from = "#${c.base10}50";
            to = "#${c.base11}50";
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
            from = "#${c.base10}";
            to = "#${c.base11}";
            angle = 45;
            in' = "oklch shorter hue";
          };
          inactive.color = "#${c.base02}";
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
        default-column-display = "tabbed";

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
          color = c.base10;
          inactive-color = c.base11;
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

      animations.window-resize.custom-shader = ''
        vec4 resize_color(vec3 coords_curr_geo, vec3 size_curr_geo) {
          vec3 coords_next_geo = niri_curr_geo_to_next_geo * coords_curr_geo;

          vec3 coords_stretch = niri_geo_to_tex_next * coords_curr_geo;
          vec3 coords_crop = niri_geo_to_tex_next * coords_next_geo;

          // We can crop if the current window size is smaller than the next window
          // size. One way to tell is by comparing to 1.0 the X and Y scaling
          // coefficients in the current-to-next transformation matrix.
          bool can_crop_by_x = niri_curr_geo_to_next_geo[0][0] <= 1.0;
          bool can_crop_by_y = niri_curr_geo_to_next_geo[1][1] <= 1.0;

          vec3 coords = coords_stretch;
          if (can_crop_by_x)
              coords.x = coords_crop.x;
          if (can_crop_by_y)
              coords.y = coords_crop.y;

          vec4 color = texture2D(niri_tex_next, coords.st);

          // However, when we crop, we also want to crop out anything outside the
          // current geometry. This is because the area of the shader is unspecified
          // and usually bigger than the current geometry, so if we don't fill pixels
          // outside with transparency, the texture will leak out.
          //
          // When stretching, this is not an issue because the area outside will
          // correspond to client-side decoration shadows, which are already supposed
          // to be outside.
          if (can_crop_by_x && (coords_curr_geo.x < 0.0 || 1.0 < coords_curr_geo.x))
              color = vec4(0.0);
          if (can_crop_by_y && (coords_curr_geo.y < 0.0 || 1.0 < coords_curr_geo.y))
              color = vec4(0.0);

          return color;
        }
      '';

      environment = {
        NIX_AUTO_RUN = "1";
        MOZ_ENABLE_WAYLAND = "1";
        ANKI_WAYLAND = "1";
        NIXOS_OZONE_WL = "1";
        LIBSEAT_BACKEND = "logind";
        QT_QPA_PLATFORM = "wayland;xcb";
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
