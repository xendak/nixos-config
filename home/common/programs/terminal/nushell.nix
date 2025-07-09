{ config, pkgs, ... }:
{
  programs = {

    carapace = {
      enable = true;
      enableNushellIntegration = true;
    };
    starship.enable = true;
    starship.enableNushellIntegration = true;
    yazi.enableNushellIntegration = true;
    zoxide.enableNushellIntegration = true;
    direnv.enableNushellIntegration = true;

    nushell = {
      enable = true;
      shellAliases = {
        ll = "ls";
        lg = "lazygit";
        y = "${pkgs.yazi}/bin/yazi";
        fz = "fzf --bind 'enter:become(hx {})'";
        cat = "${pkgs.bat}/bin/bat";
        df = "${pkgs.duf}/bin/duf";
        find = "${pkgs.fd}/bin/fd";
        grep = "${pkgs.ripgrep}/bin/rg";
        tree = "${pkgs.eza}/bin/eza --git --icons --tree";
      };
      extraConfig = # nu
        ''
          let fish_completer = {|spans|
            fish --command $'complete "--do-complete=($spans | str join " ")"'
            | from tsv --flexible --noheaders --no-infer
            | rename value description
          }

          let carapace_completer = {|spans: list<string>|
            carapace $spans.0 nushell ...$spans
            | from json
            | if ($in | default [] | where value =~ '^-.*ERR$' | is-empty) { $in } else { null }
          }

          let zoxide_completer = {|spans|
            $spans | skip 1 | zoxide query -l ...$in | lines | where {|x| $x != $env.PWD}
          }

          let external_completer = {|spans|
            let expanded_alias = scope aliases
            | where name == $spans.0
            | get -i 0.expansion

            let spans = if $expanded_alias != null {
              $spans
              | skip 1
              | prepend ($expanded_alias | split row ' ' | take 1)
            } else {
              $spans
            }

            match $spans.0 {
              # carapace completions are incorrect for nu
              nu => $fish_completer
              # fish completes commits and branch names in a nicer way
              git => $fish_completer
              # carapace doesn't have completions for asdf
              asdf => $fish_completer
              # use zoxide completions for zoxide commands
              __zoxide_z | __zoxide_zi => $zoxide_completer
              _ => $carapace_completer
            } | do $in $spans
          } 

          $env.PATH = ($env.PATH | 
            split row (char esep) |
            prepend /home/${config.home.username}/Flake/bin |
            append /usr/bin/env
          )

          def "nsp" [search_terms: string] {
            ^nix search nixpkgs $search_terms --json | from json | items {|key, value|
                {
                  name: $value.pname,
                  description: $value.description
                }
            } | where ($it.name | str downcase) =~ ($search_terms | str downcase)
            | enumerate
            | each {|item|
              if ($item.index mod 2) == 0 {
                {
                  name: $item.item.name,
                  description: $item.item.description
                  # name: $"(ansi green)($item.item.name)(ansi reset)",
                  # description: $"(ansi green)($item.item.description)(ansi reset)"
                }
              } else {
                {
                  name: $"(ansi blue)($item.item.name)(ansi reset)",
                  description: $"(ansi blue)($item.item.description)(ansi reset)"
                }
              }
            }
          }

          def "nspl" [...search_terms: string] {
          ^nix search nixpkgs ...$search_terms --json | from json | items {|key, value|
                [
                  $"($value.pname)",
                  $"($value.description)",
                  "────────────────────────────────────────"
                ] | str join "\n"
            } | each {|item| print $item}
          }


          def "nix-pkgs" [] {
              open /home/${config.home.username}/Flake/bin/nixpkgs_list | lines
          }

          def "ns" [package: string@nix-pkgs] {
              ^nix-shell -p $package --run nu
          }

          def "nr" [package: string@nix-pkgs] {
              ^nix run $"nixpkgs#($package)"
          }


          $env.EDITOR = "hx"
          $env.VISUAL = "hx"
          $env.config.buffer_editor = "hx"

          $env.config.cursor_shape.vi_insert = "blink_line"
          $env.config.cursor_shape.vi_normal = "blink_block"
          $env.PROMPT_INDICATOR_VI_NORMAL = ""
          $env.PROMPT_INDICATOR_VI_INSERT = ""
          $env.PROMPT_MULTILINE_INDICATOR = ""

          $env.config = {
            edit_mode: vi,
            show_banner: false,
            shell_integration: {
              osc2: false,
              osc7: true,
              osc8: false,
              osc133: true,
              osc633: false,
              reset_application_mode: true,
            },
            completions: {
              quick: true,
              partial: true,
              case_sensitive: false,
              algorithm: "fuzzy",
              external: {
                enable: true,
                max_results: 100,
                completer: $external_completer
              }
            },
            history: {
              sync_on_enter: true,
            },
          }
        '';
    };
  };
}
