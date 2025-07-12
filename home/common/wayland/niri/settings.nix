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
  ];

  programs.niri = {
    enable = true;
    package = pkgs.niri;
    settings = {
      # since this outputs in a sorted order we cant make proper names
      workspaces = {
        "1" = { };
        "2" = { };
        "3" = { };
        "4" = { };
        "5" = { };
      };

      prefer-no-csd = true;

      hotkey-overlay = {
        skip-at-startup = true;
      };

      layout = {
        focus-ring = {
          enable = true;
          width = 2;
          active = {
            color = c.base10;
          };
          inactive = {
            color = c.base11;
          };
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
        keyboard.xkb = {
          layout = "us";
          variant = "altgr-intl";
          options = "ctrl:nocaps";
          rules = "evdev";
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

      environment = {
        CLUTTER_BACKEND = "wayland";
        GDK_BACKEND = "wayland,x11";
        MOZ_ENABLE_WAYLAND = "1";
        NIXOS_OZONE_WL = "1";
        QT_QPA_PLATFORM = "wayland";
        QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
        ELECTRON_OZONE_PLATFORM_HINT = "auto";
        ELECTRON_ENABLE_HARDWARE_ACCELERATION = "1";

        XDG_SESSION_TYPE = "wayland";
        XDG_CURRENT_DESKTOP = "niri";
        DISPLAY = ":0";
      };
    };
  };
}
