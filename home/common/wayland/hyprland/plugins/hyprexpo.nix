{
  config,
  pkgs,
  inputs,
  ...
}: let
  c = config.colorscheme.palette;
in {
  wayland.windowManager.hyprland = {
    plugins = [
      inputs.hyprland-plugins.packages.${pkgs.system}.hyprexpo
    ];
    settings.plugin = {
      hyprexpo = {
        columns = 2;
        gap_size = config.wayland.windowManager.hyprland.settings.general.gaps_in;
        bg_col = "rgba(${c.base02}a0)";
        workspace_method = "center current";
        enable_gesture = true;
        gesture_distance = 300;
        gesture_positive = true;
      };
      bind = ["SUPERSHIFT, tab, hyprexpo:expo, toggle"];
    };
  };
}
