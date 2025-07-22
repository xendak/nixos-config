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

def qlog [] {
  qs log -n -c ($env.HOME | path join "Flake" "home" "common" "programs" "quickshell" "niri")
}

def history_search [term: string] {
    open $nu.history-path | query db $"SELECT * FROM history WHERE command_line LIKE '%($term)%'"
}

def history_delete [term: string] {
    open $nu.history-path | query db $"DELETE FROM history WHERE command_line LIKE '%($term)%'"
}

# JSON VERSION
def "nu-complete-nix-pkgs" [] {
  let cache_file = ($env.HOME | path join "Flake" "bin" "nixpkgs.json")

  if not ($cache_file | path exists) {
    return [{ value: "", description: "Cache not found. Run `update-nix-cache` first." }]
  }

  open $cache_file
  | each {
    |pkg|
      {
        value: $pkg.pname,
        description: $pkg.description
      }
    }
}

def update-nix-cache [] {
  print "Updating Nix package cache... (This might take a minute)"
  let cache_file = ($env.HOME | path join "Flake" "bin" "nixpkgs.json")
  ^nix search nixpkgs ^ --json
    | from json
    | items {|name, value| {
        pname: ($name | str replace "legacyPackages.x86_64-linux." ""),
        description: $value.description
      }
    }
    | where not ($it.pname | str downcase |str starts-with "linuxkernel")
    | where not ($it.pname | str downcase |str starts-with "androidenv")
    | where ($it.pname | str length) <= 50
    | where not ($it.pname | str downcase | str contains "plugin")
    | where not ($it.description | str downcase | str contains "kernel module")
    | where not ($it.description | str downcase | str contains "kernel driver")
    | where not ($it.description | str downcase | str contains "plugin")
    | save --force $cache_file

  print "Nix package cache is up to date."
}

# TESTING NUSHELL SQLITE
def "nu-complete-nix-pkgs-sqlite" [] {
  let db_file = ($env.HOME | path join "Flake" "bin" "nixpkgs.db")

  let sql_query = $"
    SELECT
      pname AS value,
      description
    FROM
      packages
  "

  open $db_file | query db $sql_query
}

def update-nix-cache-sqlite [] {
  let db_file = ($env.HOME | path join "Flake" "bin" "nixpkgs.db")
  if ($db_file | path exists) {
    rm $db_file
  }
  ^nix search nixpkgs ^ --json
    | from json
    | items {|key, value|
        {
          pname: ($key | str replace "legacyPackages.x86_64-linux." ""),
          description: $value.description
        }
      }
    | where not ($it.pname | str downcase | str starts-with "linuxkernel")
    | where not ($it.pname | str downcase | str starts-with "androidenv")
    | where ($it.pname | str length) <= 50
    | where not ($it.pname | str downcase | str contains "plugin")
    | where not ($it.description | str downcase | str contains "kernel module")
    | where not ($it.description | str downcase | str contains "kernel driver")
    | where not ($it.description | str downcase | str contains "plugin")
    | into sqlite $db_file --table-name packages
  open $db_file
    | query db "CREATE INDEX IF NOT EXISTS idx_pname ON packages(pname)"
    | ignore
  print $"db: ($db_file) created and indexed."
}

def nsp [search_term: string@"nu-complete-nix-pkgs-sqlite"] {
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
        name: $"(ansi white)($item.item.name)(ansi reset)",
        description: $"(ansi white)($item.item.description)(ansi reset)"
      }
    } else {
      {
        name: $"(ansi blue)($item.item.name)(ansi reset)",
        description: $"(ansi blue)($item.item.description)(ansi reset)"
      }
    }
  }
}

def nspl [...search_terms: string@"nu-complete-nix-pkgs-sqlite"] {
  ^nix search nixpkgs ...$search_terms --json | from json | items {|key, value|
      [
        $"($key)"
        $"($value.pname)",
        $"($value.description)",
        "────────────────────────────────────────"
      ] | str join "\n"
  } | each {|item| print $item}
}

def ns [...packages: string@"nu-complete-nix-pkgs-sqlite"] {
  ^nix-shell -p ...$packages --run nu
}

def nr [package: string@"nu-complete-nix-pkgs-sqlite"] {
  ^nix run $"nixpkgs#($package)"
}
