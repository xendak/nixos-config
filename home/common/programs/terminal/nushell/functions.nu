# Syncs .desktop files from Flake to user application folder
def sync-desktop-files [] {
  let source_dir = ($env.HOME | path join "Flake" "home" "common" "desktop")
  let dest_dir = ($env.HOME | path join ".local" "share" "applications")

  if not ($source_dir | path exists) {
    mkdir $source_dir
  }

  print $"Syncing desktop files..."
  print $"From: ($source_dir)"
  print $"To:   ($dest_dir)"

  ls $source_dir | where name =~ ".desktop" | each { |file|
    let filename = ($file.name | path basename)
    let target = ($dest_dir | path join $filename)
    
    ln -sf $file.name $target
  }
  
  print "Sync complete!"
}

# Creates a .desktop file for Umu/Wine applications with Gamescope/MangoHud support
def create-umu-desktop-files [
    name: string        # The name of the application
    exe: path           # The full path to the executable
    --prefix: path      # Optional: Custom Wine Prefix path
    --proton: path      # Optional: Path to specific Proton version
    --icon: path        # Optional: Custom Icon path
    --mangohud          # Enable MangoHud
    --gamescope         # Enable Gamescope
    --gamescope-args: string  # Arguments for Gamescope (e.g., "-W 1920 -H 1080 -f")
    --topology          # Enable WINE_CPU_TOPOLOGY=8:0,2,4,6,8,10,12,14
    --wayland           # Enable PROTON_ENABLE_WAYLAND=1
    --async             # Enable DXVK_ASYNC=1
    --ntsync            # Enable PROTON_USE_NTSYNC=1
] {
  let home = $env.HOME
  let flake_desktop_dir = ($home | path join "Flake" "home" "common" "desktop")

  let full_exe = ($exe | path expand)
  let full_custom_prefix = if ($prefix | is-empty) { "" } else { $prefix | path expand }
  let full_proton = if ($proton | is-empty) { "" } else { $proton | path expand }

  let safe_filename = ($name | str replace --all " " "")

  let final_prefix = if ($full_custom_prefix | is-empty) {
    $home | path join "Games" "Wine-Prefix"
  } else {
    $full_custom_prefix
  }

  let final_icon = if ($icon | is-empty) {
    $home | path join "Flake" "home" "common" "icons" $"($safe_filename).png"
  } else {
    ($safe_filename | path expand)
  }

  let e_proton = if ($full_proton | is-empty) { 
    "" 
  } else { 
    $"ProtonPath=\"($full_proton)\" " 
  }


  # Note: Gamescope requires "--" before the internal command
  let gs_cmd = if $gamescope { 
    let args = if ($gamescope_args | is-empty) { "" } else { $"($gamescope_args) " }
    $"gamescope ($args)-- " 
  } else { 
    "" 
  }

  # Note: Disables SMT i think? otherwise would be
  # WINE_CPU_TOPOLOGY=16:0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15
  let e_topology = if $topology { "WINE_CPU_TOPOLOGY=8:0,2,4,6,8,10,12,14 " } else { "" }
  let e_wayland = if $wayland { "PROTON_ENABLE_WAYLAND=1 " } else { "" }
  let e_async = if $async { "DXVK_ASYNC=1 " } else { "" }
  let e_ntsync = if $ntsync { "PROTON_USE_NTSYNC=1 " } else { "" }
  let e_mango = if $mangohud { "MANGOHUD=1 " } else { "" }

  let final_env_vars = $"($e_proton)($e_wayland)($e_async)($e_ntsync)($e_topology)($e_mango)"

  let content = $"[Desktop Entry]
Name=($name)
Exec=env WINEPREFIX=\"($final_prefix)\" ($final_env_vars)($gs_cmd)umu-run \"($full_exe)\"
Icon=($final_icon)
Type=Application
Keywords=($name)
MimeType=application/x-ms-dos-executable
StartupWMClass=($name)
"
  
  if not ($flake_desktop_dir | path exists) { mkdir $flake_desktop_dir }

  let save_path = ($flake_desktop_dir | path join $"($safe_filename).desktop")
  
  $content | save --force $save_path
  
  print $"Created definition at: ($save_path)"
  
  sync-desktop-files
}

def get-fonts [s?: string] {
    ^fc-list
    | parse "{file_path}: {names_str}:style={styles_str}"
    | each { |row|
        let names_list = ($row.names_str | split row "," | str trim)
        {
            name: ($names_list.0),
            font-file: ($row.file_path | path basename),
            # name2: ($names_list.1),
            # name3: ($names_list.2),
            style: ($row.styles_str | split row "," | str trim).0
        }
    } | uniq-by name | uniq-by font-file | sort
    
}
 
def font-search [s?: string] {
  if not ($s | is-empty) { 
    get-fonts | where ($it.name | str downcase) =~ $s
    
  } else {
    get-fonts
  }
}

def rsysd [] {
  sudo systemctl restart user@1000.service  
}

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

def --env y [...args] {
	let tmp = (mktemp -t "yazi-cwd.XXXXXX")
	yazi ...$args --cwd-file $tmp
	let cwd = (open $tmp)
	if $cwd != "" and $cwd != $env.PWD {
		cd $cwd
	}
	rm -fp $tmp
}
