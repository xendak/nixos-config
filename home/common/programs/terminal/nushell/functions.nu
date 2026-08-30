def qsc [] {
    let qsp = $env.HOME | path join "Flake" "home" "common" "programs" "quickshell" "niri"
    ps | where name like "quickshell" | each { kill $in.pid }
    qs -c $qsp -d
}

def ll [path: path = .] {
    ls -al $path | select name size mode modified user target
}

# Syncs .desktop files from Flake to user application folder
def sync-desktop-files [] {
    let source_dir = $env.HOME | path join "Flake" "home" "common" "desktop"
    let dest_dir = $env.HOME | path join ".local" "share" "applications"

    if not ($source_dir | path exists) {
        mkdir $source_dir
    }

    print $"Syncing desktop files..."
    print $"From: ($source_dir)"
    print $"To:   ($dest_dir)"

    ls $source_dir | where name =~ ".desktop" | each { |file|
    let filename = $file.name | path basename
    let target = $dest_dir | path join $filename
    
    ln -sf $file.name $target
  }

    print "Sync complete!"
}

# Download an icon from SteamGridDB
def fetch-game-icon [game_name: string, save_path: string] {
    let api_key = $env.STEAMGRIDDB

    if ($api_key | is-empty) {
        print "Warning: No STEAMGRIDDB_API_KEY found. Skipping auto-download."
        return false
    }

    print $"Searching SteamGridDB for '($game_name)'..."
    let search_game = $game_name | str replace --all " " "-"

    let search_url = $"https://www.steamgriddb.com/api/v2/search/autocomplete/($search_game)"
    let search_res = (http get --headers [Authorization $"Bearer ($api_key)"] $search_url)

    if ($search_res.data | is-empty) {
        print "Game not found on SteamGridDB."
        return false
    }

    let game_id = $search_res.data.0.id
    print $"Found Game ID: ($game_id) (($search_res.data.0.name))"

    let icon_url = $"https://www.steamgriddb.com/api/v2/icons/game/($game_id)"
    let icon_res = (http get --headers [Authorization $"Bearer ($api_key)"] $icon_url)

    if ($icon_res.data | is-empty) {
        print "No icons found for this game."
        return false
    }

    let icon = $icon_res.data.0.thumb
    print $"Downloading icon: ($icon)..."

    try {
        http get $icon | save --force $save_path
        print "Icon saved successfully."
        return true
    } catch {
        print "Failed to download icon file."
        return false
    }
}

# Completion function for documented Wine/Proton flags
def "nu-complete wine-flags" [] {
    [
        {value: "DXVK_ASYNC=1", description: "Enable asynchronous shader compilation (reduces stutter)"}
        {value: "SDL_VIDEO_DRIVER=wayland", description: "Sets the SDL backend to wayland"}
        {value: "PROTON_ENABLE_WAYLAND=1", description: "Run Wine native on Wayland"}
        {value: "PROTON_USE_NTSYNC=1", description: "Enable NT synchronization primitive (requires patched kernel)"}
        {value: "WINE_CPU_TOPOLOGY=8:0,2,4,6,8,10,12,14", description: "Restrict threads/SMT (Example for 8 cores)"}
        {value: "PROTON_DISABLE_LSTEAMCLIENT=1", description: "Disable Steam client integration (often used with Denuvo)"}
        {value: "WINEDLLOVERRIDES=\"winmm=n,b\"", description: "DLL Override example for modding/fixing"}
        {value: "WINEFSYNC=1", description: "Enable Fsync (usually on by default)"}
        {value: "PROTOWN_LOG=1", description: "Create a debug log in your home directory"}
    ]
}

# Completion function for Proton versions
def "nu-complete proton-versions" [] {
    let proton_dir = $env.HOME | path join ".local" "share" "Steam" "compatibilitytools.d"

    if ($proton_dir | path exists) {
        # Lists all directories in compatibilitytools.d and returns just their folder names
        ls $proton_dir | where type == dir | get name | path basename
    } else {
        []
    }
}

# Attaches to a running Umu/Proton pressure-vessel and launches Cheat Engine
def attach-umu-ce [
    log: path = "~/tmp/umu-run.log"                                 # Default log path
    --ce: path = "~/Games/Wine/Cheat Engine/cheatengine-x86_64.exe" # Path to your CE executable
] {
    let expanded_log = $log | path expand

    if not ($expanded_log | path exists) {
        error make {msg: $"Log file not found: ($expanded_log)"}
    }

    let log_text = (open --raw $expanded_log)

    let bus_match = $log_text | parse --regex '--bus-name=:(?P<bus>[0-9\.]+)'
    if ($bus_match | is-empty) {
        error make {msg: "Could not find --bus-name in the log."}
    }
    let bus_name = $bus_match | get bus | first

    let client_match = $log_text | parse --regex '(?P<client>/[^\s]+steam-runtime-launch-client)'
    if ($client_match | is-empty) {
        error make {msg: "Could not find steam-runtime-launch-client path in the log."}
    }
    let launch_client = $client_match | get client | first

    let ce_exec = $ce | path expand

    print $"[Info] Found Launch Client: ($launch_client)"
    print $"[Info] Attaching to Bus:    ($bus_name)"
    print $"[Info] Launching Cheat Engine..."

    ^$launch_client $"--bus-name=:($bus_name)" -- sh -c $"$PROTONPATH/proton run '($ce_exec)'"
}

# Creates a .desktop file for Umu/Wine applications with Gamescope/MangoHud support
# -- are optionals
# PRESSURE_VESSEL_SHELL=after <- enables shell for CE $PROTONPATH/run proton ./path/to/cheatengine.exe 
def create-umu-desktop-files [
    name: string              # The name of the application
    exe: path                 # The full path to the executable
    --prefix: path            # Custom Wine Prefix path
    --proton: string@"nu-complete proton-versions" # Proton version (auto-completes from compatibilitytools.d)
    --proton-detect           # Auto-detect Proton version from existing prefix
    --winenv: string@"nu-complete wine-flags"      # Auto-completes from your saved list of Wine flags
    --icon: path              # Custom Icon path (thats not in /common/icons)
    --id: string              # GAMEID from umu-database : https://github.com/Open-Wine-Components/umu-database/blob/main/umu-database.csv
    --mangohud                # Enable MangoHud
    --gamemode                # Enable Feral GameMode (gamemoderun)
    --topology                # Enable WINE_CPU_TOPOLOGY=8:0,2,4,6,8,10,12,14
    --wayland                 # Enable PROTON_ENABLE_WAYLAND=1
    --async                   # Enable DXVK_ASYNC=1
    --ntsync                  # Enable PROTON_USE_NTSYNC=1
    --custom: string          # Custom Option String
    --gamescope               # Enable Gamescope
    --gamescope-args: string  # Arguments for Gamescope (e.g., "-W 1920 -H 1080 -f")
    --wayland-max             # Force Gamescope with custom 2460x1390 max workspace config
    --denuvo                  # Enable flags for denuvo
] {
    let home = $env.HOME
    let flake_desktop_dir = $home | path join "Flake" "home" "common" "desktop"
    let denuvo_custom_dir = $home | path join ".local" "share" "Steam" "compatibilitytools.d" "DenuvOwO"
    let icons_dir = $home | path join "Flake" "home" "common" "icons"
    let proton_base_dir = $home | path join ".local" "share" "Steam" "compatibilitytools.d"

    let full_exe = $exe | path expand
    let full_custom_prefix = if ($prefix | is-empty) { "" } else {
        $prefix | path expand
    }

    # 1. Resolve Prefix first
    let final_prefix = if ($full_custom_prefix | is-empty) {
        $home | path join "Games" "Wine-Prefix"
    } else {
        $full_custom_prefix
    }

    # 2. Auto-detect Proton version if requested
    let detected_proton_name = if $proton_detect {
        let parent_version_file = $final_prefix | path dirname | path join "version"
        let pfx_version_file = $final_prefix | path join "version"

        let target_file = if ($parent_version_file | path exists) {
            $parent_version_file
        } else if ($pfx_version_file | path exists) {
            $pfx_version_file
        } else {
            ""
        }

        if not ($target_file | is-empty) {
            let version_name = (
                open --raw $target_file
                | str trim
                | split row " "
                | last
            )
            print $"[Info] Detected Proton version: ($version_name)"
            $version_name
        } else {
            print $"[Warning] --proton-detect used, but no 'version' file found in or above ($final_prefix)."
            ""
        }
    } else {
        ""
    }

    # 3. Resolve final Proton Path
    let full_proton = if not ($proton | is-empty) {
        ($proton_base_dir | path join $proton)
    } else if not ($detected_proton_name | is-empty) {
        ($proton_base_dir | path join $detected_proton_name)
    } else {
        ""
    }

    if $proton_detect and not ($full_proton | is-empty) and not ($full_proton | path exists) {
        print $"[Warning] Detected Proton path does not exist: ($full_proton)"
    }

    let safe_filename = $name | str replace --all " " ""
    let default_icon_path = $icons_dir | path join $"($safe_filename).png"

    let final_icon = if not ($icon | is-empty) {
        ($icon | path expand)
    } else {
        if ($default_icon_path | path exists) {
            $default_icon_path
        } else {
            print "Icon not found locally. Attempting auto-download..."
            let success = (fetch-game-icon $name $default_icon_path)
            if $success {
                $default_icon_path
            } else {
                ""
            }
        }
    }

    let e_proton = if $denuvo {
        $"PROTONPATH=\"($denuvo_custom_dir)\" "
    } else if ($full_proton | is-empty) {
        ""
    } else {
        $"PROTONPATH=\"($full_proton)\" "
    }

    let e_gameid = if ($id | is-empty) {
        ""
    } else {
        $"GAMEID=\"($id)\" "
    }

    # Determine Gamescope parameters
    let use_gamescope = ($gamescope or $wayland_max)

    let gs_cmd = if $use_gamescope {
        let args = if $wayland_max {
            "-f -w 2460 -h 1390 -W 2460 -H 1390 --force-grab-cursor --backend wayland "
        } else if not ($gamescope_args | is-empty) {
            $"($gamescope_args) "
        } else {
            ""
        }
        $"gamescope ($args)-- "
    } else {
        ""
    }

    let gm_cmd = if $gamemode {
        "gamemoderun "
    } else {
        ""
    }

    let e_winenv = if ($winenv | is-empty) { "" } else { $"($winenv) " }
    let e_topology = if $topology { "WINE_CPU_TOPOLOGY=8:0,2,4,6,8,10,12,14 " } else { "" }
    let e_wayland = if $wayland { "PROTON_ENABLE_WAYLAND=1 " } else { "" }
    let e_denuvo = if $denuvo { "PROTON_DISABLE_LSTEAMCLIENT=1 WINEDLLOVERRIDES=\"winmm=n,b\" " } else { "" }
    let e_async = if $async { "DXVK_ASYNC=1 " } else { "" }
    let e_ntsync = if $ntsync { "PROTON_USE_NTSYNC=1 " } else { "" }
    let e_mango = if $mangohud { "MANGOHUD=1 " } else { "" }
    let e_custom = if ($custom | is-empty) { "" } else { $"($custom) " }

    let final_env_vars = $"($e_denuvo)($e_proton)($e_wayland)($e_async)($e_ntsync)($e_topology)($e_gameid)($e_custom)($e_winenv)($e_mango)"

    let content = $"[Desktop Entry]
Name=($name)
Exec=env WINEPREFIX=\"($final_prefix)\" ($final_env_vars)($gs_cmd)($gm_cmd)umu-run \"($full_exe)\"
Icon=($final_icon)
Type=Application
Keywords=($name)
MimeType=application/x-ms-dos-executable
StartupWMClass=($name)
"

    if not ($flake_desktop_dir | path exists) { mkdir $flake_desktop_dir }

    let save_path = $flake_desktop_dir | path join $"($safe_filename).desktop"

    $content | save --force $save_path

    print $"Created definition at: ($save_path)"

    sync-desktop-files
}

def get-fonts [s?: string] {
    ^fc-list
    | parse "{file_path}: {names_str}:style={styles_str}"
    | each { |row|
        let names_list = $row.names_str | split row "," | str trim
        {
            name: ($names_list.0),
            font-file: ($row.file_path | path basename),
            # name2: ($names_list.1),
            # name3: ($names_list.2),
            style: ($row.styles_str | split row "," | str trim).0
        }
    }
    | uniq-by name
    | uniq-by font-file
    | sort
}

def font-search [s?: string] {
    if not ($s | is-empty) {
        get-fonts | where ($it.name | str lowercase) =~ $s
    } else {
        get-fonts
    }
}

def rsysd [] {
    sudo systemctl restart user@1000.service
}

def upb [
  --show-trace (-l)      # Append --show-trace to rebuild
  --no-cache (-c)        # Disable eval-cache
  --delete-old (-d)      # Collect garbage after success
  --reboot-sys (-r)      # Reboot after everything is done
  --shutdown-sys (-u)    # shutdown after everything is done
  --nom (-n)             # Disables Nix Output Monitor
] {
    cd ($env.HOME | path join "Flake")

    mut flags = (get-ssh-flags)

    if $no_cache {
        $flags = ($flags | append ["--option" "eval-cache" "false"])
    }

    if $show_trace {
        $flags = ($flags | append "--show-trace")
    }

    let host = sys host | get hostname

    if not $nom {
        sudo nixos-rebuild boot --flake $".#($host)" ...$flags --log-format internal-json -v e+o>| nom --json
    } else {
        sudo nixos-rebuild boot --flake $".#($host)" ...$flags
    }

    if $env.LAST_EXIT_CODE == 0 {
        if $delete_old {
            print "Cleaning up garbage..."
            sudo nix-collect-garbage -d
            nix-collect-garbage -d
        }

        if $reboot_sys {
            print "Rebooting system..."
            reboot
        }

        if $shutdown_sys {
            print "Shutingdown system..."
            poweroff
        }
    }
}

def upd [
  --show-trace (-l)      # Append --show-trace to rebuild
  --no-cache (-c)        # Disable eval-cache
  --delete-old (-d)      # Collect garbage after success
  --nom (-n)             # Disables Nix Output Monitor
] {
    cd ($env.HOME | path join "Flake")

    # BS home-manager doesnt replace...
    let to_del = [
        $"($env.HOME)/.config/gtk-2.0/gtkrc"
        $"($env.HOME)/.config/gtk-3.0/settings.ini"
        $"($env.HOME)/.config/gtk-4.0/gtk.css"
        $"($env.HOME)/.config/gtk-4.0/settings.ini"
        $"($env.HOME)/.config/zathura/zathurarc"
    ]
    $to_del | each {|it| if ($it | path exists) { rm $it } }

    mut flags = (get-ssh-flags)

    if $no_cache {
        $flags = ($flags | append ["--option" "eval-cache" "false"])
    }

    if $show_trace {
        $flags = ($flags | append "--show-trace")
    }

    let host = sys host | get hostname

    if not $nom {
        sudo nixos-rebuild switch --flake $".#($host)" ...$flags --log-format internal-json -v e+o>| nom --json
    } else {
        sudo nixos-rebuild switch --flake $".#($host)" ...$flags
    }

    if $env.LAST_EXIT_CODE == 0 {
        if $delete_old {
            print "Cleaning up garbage..."
            sudo nix-collect-garbage -d
            nix-collect-garbage -d
        }
    }
}

def up [
  --boot (-b)            # Run 'boot' instead of 'switch' (default is switch)
  --update-flake (-f)    # Update flake.lock inputs before building
  --show-trace (-l)      # Append --show-trace to rebuild
  --no-cache (-c)        # Disable eval-cache
  --delete-old (-d)      # Collect garbage after success (keeps last 2)
  --reboot-sys (-r)      # Reboot after everything is done
  --shutdown-sys (-u)    # Shutdown after everything is done
  --nom (-n)             # Disables Nix Output Monitor
] {
    let flake_dir = $env.HOME | path join "Flake"
    let host = sys host | get hostname
    let target = $"($flake_dir)#($host)"

    let action = if $boot { "boot" } else { "switch" }

    let to_del = [
        $"($env.HOME)/.config/gtk-2.0/gtkrc"
        $"($env.HOME)/.config/gtk-3.0/settings.ini"
        $"($env.HOME)/.config/gtk-4.0/gtk.css"
        $"($env.HOME)/.config/gtk-4.0/settings.ini"
        $"($env.HOME)/.config/zathura/zathurarc"
    ]
    $to_del | each {|it| if ($it | path exists) { rm $it } }

    mut nh_args = []
    if $nom {
        $nh_args = ($nh_args | append "--no-nom")
    }
    if $update_flake {
        $nh_args = ($nh_args | append "--update")
    }

    mut nix_args = (get-ssh-flags)
    if $no_cache {
        $nix_args = ($nix_args | append ["--option" "eval-cache" "false"])
    }
    if $show_trace {
        $nix_args = ($nix_args | append "--show-trace")
    }

    print $"Running nh os ($action) for ($host)..."
    nh os $action ...$nh_args $target -- ...$nix_args

    if $env.LAST_EXIT_CODE == 0 {
        if $delete_old {
            print "Cleaning up garbage (keeping last 2 generations)..."
            nh clean all --keep 2
        }

        if $reboot_sys {
            print "Rebooting system..."
            reboot
        }

        if $shutdown_sys {
            print "Shutting down system..."
            poweroff
        }
    }
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

def "nu-complete-nix-pkgs-sqlite" [] {
    let db_file = $env.HOME | path join "Flake" "bin" "nixpkgs.db"

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
    let db_file = $env.HOME | path join "Flake" "bin" "nixpkgs.db"
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
    | where not ($it.pname | str lowercase | str starts-with "linuxkernel")
    | where not ($it.pname | str lowercase | str starts-with "androidenv")
    | where ($it.pname | str length) <= 50
    | where not ($it.pname | str lowercase | str contains "plugin")
    | where not ($it.description | str lowercase | str contains "kernel module")
    | where not ($it.description | str lowercase | str contains "kernel driver")
    | where not ($it.description | str lowercase | str contains "plugin")
    | into sqlite $db_file --table-name packages
    open $db_file
    | query db "CREATE INDEX IF NOT EXISTS idx_pname ON packages(pname)"
    | ignore
    print $"db: ($db_file) created and indexed."
}

def nsp [search_term: string@"nu-complete-nix-pkgs-sqlite"] {
    ^nix search nixpkgs $search_term --json
    | from json
    | items {|key, value|
      {
        name: $value.pname,
        description: $value.description
      }
  }
    | where ($it.name | str lowercase) =~ ($search_term | str lowercase)
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

def dev [...args] {
    if ("flake.nix" | path exists) {
        let flags = (get-ssh-flags)
        ^nix develop ...$flags ...$args
    } else if ("shell.nix" | path exists) {
        let flags = (get-ssh-flags)
        ^nix-shell ...$flags ...$args --run nu
    } else {
        print $"(ansi red)Error:(ansi reset) failed to find (ansi green)flake.nix(ansi reset) or (ansi green)shell.nix(ansi reset)"
    }
}

def ns [--unfree(-f), ...packages: string@"nu-complete-nix-pkgs-sqlite"] {
    let env_vars = {NIXPKGS_ALLOW_UNFREE: "1"}
    let pkg_refs = $packages | each {|it| $"nixpkgs#($it)" }

    with-env $env_vars {
    if $unfree {
      ^nix shell --impure ...$pkg_refs ...(get-ssh-flags) --command nu
    } else {
      ^nix shell ...$pkg_refs ...(get-ssh-flags) --command nu
    }
  }
}

def nr [package: string@"nu-complete-nix-pkgs-sqlite", --unfree(-f)] {
    let env_vars = {NIXPKGS_ALLOW_UNFREE: "1"}
    with-env $env_vars {
    if $unfree {
      ^nix run --impure $"nixpkgs#($package)" ...(get-ssh-flags) 
    } else {
      ^nix run $"nixpkgs#($package)" ...(get-ssh-flags) 
    }
  }
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

def get-ssh-flags [] {
    let host = sys host | get hostname
    if $host == "Snow" { return [] }

    let snow_up = (nc -z -w 1 Snow 22 | complete | get exit_code) == 0

    if $snow_up {
        print $"❄️ Snow is (ansi green)online(ansi reset)..."
        return [
            "--option" "extra-substituters"
            "ssh-ng://xendak@Snow?ssh-key=/etc/ssh/ssh_host_ed25519_key"
            "--option" "extra-trusted-public-keys"
            "Snow-1:ePOd1J2YyhEQZjXK3t/yA5Nt3aWFo4Bdp3ibjtW6Lpo="
            "--option" "max-jobs"
            "0"
        ]
    } else {
        print $"❄️ Snow is (ansi red)offline(ansi reset)..."
        return [
            "--option" "builders"
            ""
            "--option" "max-jobs"
            "auto"
            "--option" "substituters"
            "https://cache.nixos.org https://nix-community.cachix.org"
        ]
    }
}

def "nu-complete vynta-themes" [] {
    let output = vynta | complete | get stdout

    $output
    | lines
    | skip until {|it| $it =~ "Available themes:"}
    | skip 1
    | str join " "
    | split row -r '\s+'
    | where ($it | str length) > 0
}
export extern "vynta" [
    theme?: string@"nu-complete vynta-themes"
]
