{
  ...
}:
let
  border-radius = 12.0;
in
{
  programs.niri.settings = {
    layer-rules = [
      {
        matches = [
          {
            namespace = "^swww-daemon$";
          }
        ];
        # place-within-backdrop = true;
      }
    ];
    window-rules = [
      {
        matches = [
          { app-id = "io.github.waylyrics.Waylyrics"; }
        ];
        open-floating = true;
        open-focused = false;
        default-column-width = {
          proportion = 1.0;
        };
        min-height = 30;
        max-height = 100;
        default-float-position = {
          x = 0;
          y = 0;
          relative-to = "top";
        };
        block-out-from = "screen-capture";
        shadow = {
          off = true;
        };
        focus-ring = {
          off = true;
        };
        border = {
          off = true;
        };
      }
      {
        matches = [
          { app-id = "pavucontrol"; }
          { app-id = "f_terminal"; }
          { app-id = "mpv"; }
          { app-id = "Winetricks"; }
          { app-id = "deluge"; }
          { app-id = "rustdesk"; }
          { app-id = "steam"; }
          { app-id = "org.kde.dolphin"; }
          { title = ".*Bitwarden.*"; }
          { title = "Picture-in-Picture"; }
        ];
        open-floating = true;
      }
      {
        matches = [
          { app-id = "firefox"; }
          { app-id = "zen-browser"; }
          { app-id = "zen-beta"; }
          { app-id = "chromium"; }
        ];
        open-on-workspace = "2";
      }
      {
        matches = [
          { app-id = "vesktop"; }
          { app-id = "com.github.th_ch.youtube_music"; }
        ];
        open-on-workspace = "3";
        open-maximized = true;
      }
      {
        matches = [
          { app-id = "steam"; }
          { app-id = "lutris"; }
        ];
        open-on-workspace = "5";
      }

      {
        matches = [ { } ];
        geometry-corner-radius = {
          top-left = border-radius;
          top-right = border-radius;
          bottom-left = border-radius;
          bottom-right = border-radius;
        };
        clip-to-geometry = true;
      }
    ];
  };
}
