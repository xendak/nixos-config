{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  c = config.colorscheme.palette;
  hyprbars =
    (inputs.hyprland-plugins.packages.${pkgs.stdenv.hostPlatform.system}.hyprbars.override {
      # Make sure it's using the same hyprland package as we are
      hyprland = config.wayland.windowManager.hyprland.package;
    }).overrideAttrs
      (old: {
        # Yeet the initialization notification (I hate it)
        postPatch = (old.postPatch or "") + ''
          ${lib.getExe pkgs.gnused} -i '/Initialized successfully/d' main.cpp
        '';
        # patches = (old.patches or [ ]) ++ [ ./barDeco.patch ];
      });
in
{
  wayland.windowManager.hyprland = {
    plugins = [
      hyprbars
    ];
    settings.plugin = {
      hyprbars = {
        bar_color = "rgba(${c.base02}ff)";
        bar_height = 28;
        "col.text" = "rgba(${c.base05}ff)";
        bar_text_size = 12;
        bar_text_font = "Sofia Pro";
        bar_precedence_over_border = true;
        bar_part_of_window = true;
        buttons = {
          button_size = 0;
          hyprbars-button = [
            "rgba(${c.base02}00), 26,◬, hyprctl dispatch killactive"
            "rgba(${c.base02}00), 26,▽, hyprctl dispatch fullscreen 1"
          ];
        };
      };
      bind =
        let
          barsEnabled = "hyprctl -j getoption plugin:hyprbars:bar_height | ${lib.getExe pkgs.jq} -re '.int != 0'";
          setBarHeight = height: "hyprctl keyword plugin:hyprbars:bar_height ${toString height}";
          toggleOn = setBarHeight config.wayland.windowManager.hyprland.settings.plugin.hyprbars.bar_height;
          toggleOff = setBarHeight 0;
        in
        [
          "SUPERSHIFT,T,exec,${barsEnabled} && ${toggleOff} || ${toggleOn}"
        ];
    };
  };
}
