{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  historyPath = "/home/${config.home.username}/.cache/gobuild/history.json";
  functionsLua = lib.replaceStrings [ "@HISTORY_PATH@" ] [ historyPath ] (
    builtins.readFile ./functions.lua
  );
in

{
  home.sessionVariables.TERMINAL = lib.mkDefault "wezterm";
  home.sessionPath = [ "$HOME/Flake/bin" ];

  home.persistence = {
    "/persist" = {
      directories = [
        ".cache/gobuild/"
      ];
    };
  };

  programs.wezterm = {
    enable = true;
    package = inputs.wezterm-floating.packages.${pkgs.stdenv.hostPlatform.system}.default;
    extraConfig = # lua
      ''
        local config = wezterm.config_builder()
        local color = dofile(wezterm.config_dir .. '/colors/current.lua')

        local funcs = require('functions')
        local keys = require('keys')

        config.colors = color
        config.font = wezterm.font_with_fallback {
          { family = '${config.fontProfiles.monospace.family}', weight = 'Regular' },
          'Source Han Code JP',
          'Noto Color Emoji', 
        }
        config.font_size = 12.0
        config.color_scheme = "Custom"
        config.window_padding = { left = 15, right = 15, top = 15, bottom = 15 }
        config.hide_tab_bar_if_only_one_tab = true
        config.window_close_confirmation = "NeverPrompt"
        config.enable_wayland = true

        config.keys = keys.get_keys(funcs)

        return config
      '';
  };

  xdg.configFile."wezterm/functions.lua".text = functionsLua;
  xdg.configFile."wezterm/keys.lua".source = ./keys.lua;
}
