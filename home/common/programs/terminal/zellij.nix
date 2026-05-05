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
        show_startup_tips false
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
                welcome_screen false
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
         
        keybinds {
            tmux clear-defaults=true {
                bind "Ctrl f" { Write 2; SwitchToMode "Normal"; }
                bind "Esc" { SwitchToMode "Normal"; }
                bind "g" { SwitchToMode "Locked"; }
                bind "p" { SwitchToMode "Pane"; }
                bind "t" { SwitchToMode "Tab"; }
                bind "n" { SwitchToMode "Resize"; }
                bind "h" { SwitchToMode "Move"; }
                bind "s" { SwitchToMode "Scroll"; }
                bind "o" { SwitchToMode "Session"; }
                bind "q" { Quit; }
            }
            normal clear-defaults=true {
                bind "Ctrl f" { SwitchToMode "Tmux"; }
                bind "Ctrl s" { SwitchToMode "Search"; }
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
