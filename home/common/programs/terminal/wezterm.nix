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
  defaultColorScheme = config.colorscheme;
  # TODO: find a way to do this.
  # darkColorScheme = config.dark.colorscheme;
  # lightColorScheme = config.light.colorscheme;
  darkColorScheme = import ../../colors/luna.nix;
  lightColorScheme = import ../../colors/grayscale-nier.nix;

  mkWeztermTheme = colorscheme: ''
    return {
      foreground = "#${colorscheme.palette.base05}",
      background = "#${colorscheme.palette.base00}",
      cursor_bg = "#F36",
      cursor_fg = "#000",
      cursor_border = "#${colorscheme.palette.base05}",
      selection_fg = "#${colorscheme.palette.base00}",
      selection_bg = "#${colorscheme.palette.base05}",
      ansi = {
        "#${colorscheme.palette.base00}",
        "#${colorscheme.palette.base08}",
        "#${colorscheme.palette.base0B}",
        "#${colorscheme.palette.base0A}",
        "#${colorscheme.palette.base0D}",
        "#${colorscheme.palette.base0E}",
        "#${colorscheme.palette.base0C}",
        "#${colorscheme.palette.base05}",
      },
      brights = {
        "#${colorscheme.palette.base03}",
        "#${colorscheme.palette.base08}",
        "#${colorscheme.palette.base0B}",
        "#${colorscheme.palette.base0A}",
        "#${colorscheme.palette.base0D}",
        "#${colorscheme.palette.base0E}",
        "#${colorscheme.palette.base0C}",
        "#${colorscheme.palette.base07}",
      },
      tab_bar = {
        background = "#${colorscheme.palette.base01}",
        active_tab = {
          bg_color = "#${colorscheme.palette.base00}",
          fg_color = "#${colorscheme.palette.base05}",
        },
        inactive_tab = {
          bg_color = "#${colorscheme.palette.base01}",
          fg_color = "#${colorscheme.palette.base04}",
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
      ".config/wezterm/colors/default.lua".text = mkWeztermTheme defaultColorScheme;
      ".config/wezterm/colors/dark.lua".text = mkWeztermTheme darkColorScheme.colorScheme;
      ".config/wezterm/colors/light.lua".text = mkWeztermTheme lightColorScheme.colorScheme;
    };
  };

  # Wezterm configuration
  programs.wezterm = {
    enable = true;
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
