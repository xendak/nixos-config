def upb [...args: string] {
  cd ($env.HOME | path join "Flake")
  sudo nixos-rebuild boot --flake .#($env.USER) --show-trace ...$args
}

def upd [...args: string] {
  cd ($env.HOME | path join "Flake")
  sudo nixos-rebuild switch --flake .#($env.USER) --show-trace ...$args
}

def et [...args: string] {
  emacsclient -t ...$args 
}

def ecr [...args: string] {
  emacsclient -c -r ...$args
}

def ecs [...args: string] {
  emacsclient -c -a emacs ...$args
}

def history_search [term: string] {
    open $nu.history-path | query db $"SELECT * FROM history WHERE command_line LIKE '%($term)%'"
}

def history_delete [term: string] {
    open $nu.history-path | query db $"DELETE FROM history WHERE command_line LIKE '%($term)%'"
}

def nsp [search_term: string] {
  ^nix search nixpkgs $search_term --json | from json | items {|key, value|
      {
        name: $value.pname,
        description: $value.description
      }
  } | where ($it.name | str downcase) =~ ($search_term | str downcase)
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

def nspl [...search_terms: string] {
  ^nix search nixpkgs ...$search_terms --json | from json | items {|key, value|
      [
        $"($key)"
        $"($value.pname)",
        $"($value.description)",
        "────────────────────────────────────────"
      ] | str join "\n"
  } | each {|item| print $item}
}

def nix-pkgs [] {
  open ($env.HOME | path join "/Flake/bin/nixpkgs_list") | lines
}

def ns [...packages: string@nix-pkgs] {
  ^nix-shell -p ...$packages --run nu
}

def nr [package: string@nix-pkgs] {
  ^nix run $"nixpkgs#($package)"
}
