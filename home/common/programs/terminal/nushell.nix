{ config, pkgs, ... }:
{
  home.persistence."/persist/home/${config.home.username}".files = [
    ".config/nushell/history.txt"
    ".config/nushell/history.sqlite3"
    ".config/nushell/history.sqlite3-shm"
    ".config/nushell/history.sqlite3-wal"
  ];

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
              nu => $fish_completer
              git => $fish_completer
              asdf => $fish_completer
              __zoxide_z | __zoxide_zi => $zoxide_completer
              _ => $fish_completer
            } | do $in $spans
          } 

          $env.PATH = ($env.PATH | 
            split row (char esep) |
            prepend /home/${config.home.username}/Flake/bin |
            append /usr/bin/env
          )

          def history_search [term: string] {
              open $nu.history-path | query db $"SELECT * FROM history WHERE command_line LIKE '%($term)%'"
          }
          def history_delete [term: string] {
              open $nu.history-path | query db $"DELETE FROM history WHERE command_line LIKE '%($term)%'"
          }

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
                  $"($key)"
                  $"($value.pname)",
                  $"($value.description)",
                  "────────────────────────────────────────"
                ] | str join "\n"
            } | each {|item| print $item}
          }


          def "nix-pkgs" [] {
              open /home/${config.home.username}/Flake/bin/nixpkgs_list | lines
          }

          def "ns" [...packages: string@nix-pkgs] {
              ^nix-shell -p ...$packages --run nu
          }

          def "nr" [package: string@nix-pkgs] {
              ^nix run $"nixpkgs#($package)"
          }

          # ai key
          let secret_path = ($env.HOME | path join '.ssh/gemini')
          if ($secret_path | path exists) {
              $env.GEMINI_API_KEY = (open $secret_path | str trim)
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
              sort: smart,
              case_sensitive: false,
              algorithm: "prefix",
              external: {
                enable: true,
                max_results: 100,
                completer: $external_completer
              }
            },
            history: {
              file_format: sqlite,
              max_size: 1_000_000,
              sync_on_enter: true,
              isolation: false,
            },
          }
        '';
    };
  };
}
