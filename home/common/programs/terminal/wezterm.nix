{
  inputs,
  config,
  pkgs,
  lib,
  ...
}:
let
  # inherit (config.colorscheme) palette;
  xterm = {
    wezterm = pkgs.writeShellScriptBin "xterm" ''
      ${pkgs.wezterm}/bin/wezterm "$@"
    '';
  };

  mkWeztermTheme =
    palette: with palette; ''
      return {
        foreground = "${base05}",
        background = "${base00}",
        cursor_bg = "#F36",
        cursor_fg = "#000",
        cursor_border = "${base05}",
        selection_fg = "${base00}",
        selection_bg = "${base05}",
        ansi = {
          "${base00}",
          "${base08}",
          "${base0B}",
          "${base0A}",
          "${base0D}",
          "${base0E}",
          "${base0C}",
          "${base05}",
        },
        brights = {
          "${base03}",
          "${base08}",
          "${base0B}",
          "${base0A}",
          "${base0D}",
          "${base0E}",
          "${base0C}",
          "${base07}",
        },
        tab_bar = {
          background = "${base01}",
          active_tab = {
            bg_color = "${base00}",
            fg_color = "${base05}",
          },
          inactive_tab = {
            bg_color = "${base01}",
            fg_color = "${base04}",
          },
        },
      }
    '';
in
{
  home = {
    packages = [
      xterm.wezterm
    ];
    sessionVariables = {
      TERMINAL = lib.mkForce "wezterm";
    };
    sessionPath = [ "$HOME/Flake/bin" ];
    file = {
      ".config/wezterm/colors/default.lua".text =
        mkWeztermTheme config.themes.default.colorScheme.palette;
      ".config/wezterm/colors/dark.lua".text = mkWeztermTheme config.themes.dark.colorScheme.palette;
      ".config/wezterm/colors/light.lua".text = mkWeztermTheme config.themes.light.colorScheme.palette;
    };
  };

  # Wezterm configuration
  programs.wezterm = {
    enable = true;
    package = pkgs.wezterm;
    # package = inputs.wezterm.packages.${pkgs.system}.default;
    extraConfig = ''
      local color = dofile(wezterm.config_dir .. '/colors/current.lua')
      wezterm.add_to_config_reload_watch_list(wezterm.config_dir .. '/colors/current.lua')
      local config = {}

      config.colors = color
      -- config.font = wezterm.font("${config.fontProfiles.monospace.family}", {weight = 'Regular'})
      config.font = wezterm.font_with_fallback {
       { family = '${config.fontProfiles.monospace.family}', weight = 'Regular' },
       'Source Han Code JP',
       'Noto Color Emoji', 
      }

      config.font_size = 12.0
      config.color_scheme = "Custom"
      config.window_padding = {
        left = 15,
        right = 15,
        top = 15,
        bottom = 15,
      }
      config.hide_tab_bar_if_only_one_tab = true
      config.window_close_confirmation = "NeverPrompt"
      config.check_for_updates = false
      config.automatically_reload_config = true
      config.enable_wayland = true
      config.keys = {
        { key = 's', mods = 'CTRL|ALT', action = wezterm.action.SpawnTab("CurrentPaneDomain") },
        -- movement
        { key="o", mods="ALT", action=wezterm.action{PaneSelect={}} },
        { key = "q",  mods = "CTRL|SHIFT", action = wezterm.action({ ActivateTabRelative = -1 }) },
        { key = "e",  mods = "CTRL|SHIFT", action = wezterm.action({ ActivateTabRelative =  1 }) },
        { key = "a",  mods = "ALT", action = wezterm.action.ActivatePaneDirection 'Left' },
        { key = "d",  mods = "ALT", action = wezterm.action.ActivatePaneDirection 'Right' },
        { key = "w",  mods = "ALT", action = wezterm.action.ActivatePaneDirection 'Up' },
        { key = "s",  mods = "ALT", action = wezterm.action.ActivatePaneDirection 'Down' },
        { key = 'w',  mods = "CTRL|SHIFT", action = wezterm.action.CloseCurrentPane { confirm = false } },
        { key = '\\', mods = 'ALT', action = wezterm.action_callback(function(_, pane)
                  local tab = pane:tab()
                  local panes = tab:panes_with_info()
                  if #panes == 1 then
                      pane:split({
                          direction = "Bottom",
                          size = 0.35,
                      })
                  elseif not panes[1].is_zoomed then
                      panes[1].pane:activate()
                      tab:set_zoomed(true)
                  elseif panes[1].is_zoomed then
                      tab:set_zoomed(false)
                      panes[2].pane:activate()
                  end
              end),
        },
        { key = 'h', mods = 'ALT', action = wezterm.action_callback(function(_, pane)
                  local tab = pane:tab()
                  local panes = tab:panes_with_info()
                  if #panes == 1 then
                      pane:split({
                          direction = "Right",
                          size = 0.35,
                      })
                  elseif not panes[1].is_zoomed then
                      panes[1].pane:activate()
                      tab:set_zoomed(true)
                  elseif panes[1].is_zoomed then
                      tab:set_zoomed(false)
                      panes[2].pane:activate()
                  end
              end),
        },
        { key="Backspace", mods="CTRL", action=wezterm.action{SendString="\x17"} }
      }
      return config
    '';
  };
}
