{ pkgs, inputs, ... }:
{
  home.packages = [
    pkgs.zellij
  ];

  xdg.configFile."zellij/layouts/default.kdl".source =
    pkgs.writeText "default.kdl"
      # kdl
      ''
        layout {
            pane name="Editor" focus=true
            pane size=1 borderless=true {
                plugin location="zellij:status-bar"
            }
        }
      '';

  xdg.configFile."zellij/config.kdl".source =
    pkgs.writeText "config.kdl"
      # kdl
      ''
        plugins {
            about location="zellij:about"
            compact-bar location="zellij:compact-bar"
            configuration location="zellij:configuration"
            filepicker location="zellij:strider" {
                cwd "/"
            }
            plugin-manager location="zellij:plugin-manager"
            session-manager location="zellij:session-manager"
            status-bar location="zellij:status-bar"
            strider location="zellij:strider"
            tab-bar location="zellij:tab-bar"
            welcome-screen location="zellij:session-manager" {
                welcome_screen true
            }
        }

        load_plugins {
            zellij:link
            "file:${
              inputs.zellij-pane-toggle.packages.${pkgs.stdenv.hostPlatform.system}.default
            }/lib/zellij-pane-toggle.wasm" {
                _allow_exec_host_cmd true
            }
        }
        web_client {
            font "monospace"
        }
         
        keybinds clear-defaults=true {
            shared_except "locked" {
                bind "Alt w" { MoveFocus "Up"; }
                bind "Alt a" { MoveFocus "Left"; }
                bind "Alt s" { MoveFocus "Down"; }
                bind "Alt d" { MoveFocus "Right"; }

                bind "Alt j" {
                    MessagePlugin "file:${
                      inputs.zellij-pane-toggle.packages.${pkgs.stdenv.hostPlatform.system}.default
                    }/lib/zellij-pane-toggle.wasm" {
                        name "toggle"
                        payload "float"
                    }
                }
                bind "Alt h" {
                    MessagePlugin "file:${
                      inputs.zellij-pane-toggle.packages.${pkgs.stdenv.hostPlatform.system}.default
                    }/lib/zellij-pane-toggle.wasm" {
                        name "toggle"
                        payload "vertical"
                    }
                }
                bind "Alt v" {
                    MessagePlugin "file:${
                      inputs.zellij-pane-toggle.packages.${pkgs.stdenv.hostPlatform.system}.default
                    }/lib/zellij-pane-toggle.wasm" {
                        name "toggle"
                        payload "horizontal"
                    }
                }
                bind "Alt q" { Quit; }
            }
        }
      '';

  home.persistence = {
    "/persist" = {
      directories = [
        # ".config/zellij"
        ".local/cache/zellij"
      ];
    };
  };
}
