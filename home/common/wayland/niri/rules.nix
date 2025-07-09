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
          { app-id = "zen-beta"; }
        ];
        open-on-workspace = "2";
        open-maximized = true;
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
