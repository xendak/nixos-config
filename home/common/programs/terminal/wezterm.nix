{
  inputs,
  config,
  pkgs,
  lib,
  ...
}:
let
  inherit (config.colorscheme) palette;
  xterm = {
    wezterm = pkgs.writeShellScriptBin "xterm" ''
      ${pkgs.wezterm}/bin/wezterm "$@"
    '';
  };
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
  };

  # Wezterm configuration
  programs.wezterm = {
    enable = true;
    # package = inputs.wezterm.packages.${pkgs.system}.default;
    extraConfig = ''
      return {
        font = wezterm.font("${config.fontProfiles.monospace.family}",
          {weight = 'Regular'}),
        font_size = 12.0,
        color_scheme = "Custom",
        window_padding = {
          left = 15,
          right = 15,
          top = 15,
          bottom = 15,
        },
        hide_tab_bar_if_only_one_tab = true,
        window_close_confirmation = "NeverPrompt",
        check_for_updates = false,
        automatically_reload_config = true,
        enable_wayland = true,
        colors = {
          foreground = "#${palette.base05}",
          background = "#${palette.base00}",
          cursor_bg = "#F36",
          cursor_fg = "#000",
          cursor_border = "#${palette.base05}",
          selection_fg = "#${palette.base00}",
          selection_bg = "#${palette.base05}",
          ansi = {
            "#${palette.base00}",
            "#${palette.base08}",
            "#${palette.base0B}",
            "#${palette.base0A}",
            "#${palette.base0D}",
            "#${palette.base0E}",
            "#${palette.base0C}",
            "#${palette.base05}",
          },
          brights = {
            "#${palette.base03}",
            "#${palette.base08}",
            "#${palette.base0B}",
            "#${palette.base0A}",
            "#${palette.base0D}",
            "#${palette.base0E}",
            "#${palette.base0C}",
            "#${palette.base07}",
          },
          tab_bar = {
            background = "#${palette.base01}",
            active_tab = {
              bg_color = "#${palette.base00}",
              fg_color = "#${palette.base05}",
            },
            inactive_tab = {
              bg_color = "#${palette.base01}",
              fg_color = "#${palette.base04}",
            },
          },
        },
        keys = {
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
      }
    '';
  };
}
