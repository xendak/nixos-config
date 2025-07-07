{ config, pkgs, ... }:
let
  c = config.colorscheme.palette;
in
{
  home.packages = [
    pkgs.xwayland-satellite
  ];

  programs.niri = {
    enable = true;
    package = pkgs.niri;
    settings = {
      workspaces = {
        "workspace" = { };
        "browser" = { };
        "vesktop" = { };
      };

      prefer-no-csd = true;

      hotkey-overlay = {
        skip-at-startup = true;
      };

      layout = {

        focus-ring = {
          enable = true;
          width = 3;
          active = {
            color = c.base10;
          };
          inactive = {
            color = c.base11;
          };
        };

        gaps = 6;

        struts = {
          left = 20;
          right = 20;
          top = 20;
          bottom = 20;
        };
      };

      input = {
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
        focus-follows-mouse.enable = true;
        warp-mouse-to-focus.enable = false;
      };

      outputs = {
        "DP-1" = {
          mode = {
            width = 2560;
            height = 1440;
            refresh = 144;
          };
          scale = 1.0;
          position = {
            x = 0;
            y = 0;
          };
        };
      };

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
