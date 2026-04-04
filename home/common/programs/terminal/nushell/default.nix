{
  config,
  host,
  pkgs,
  lib,
  ...
}:
let
  # prepend windows PATH if we're inside wsl
  # done outside because we might have a ntfs parition mounted at /mnt/c/
  win_block =
    if config.home.username == "nixos" then
      ''
        try {
          let windows_paths = ^/mnt/c/Windows/System32/cmd.exe /c "echo %PATH%" o+e>| split row ";" | each { |t| wslpath -u $t } | reject 0
          [
            ($windows_paths | where ($it | str downcase) =~ 'windows' | first),
            ($windows_paths | where ($it | str downcase) =~ 'cmd' | first),
            ($windows_paths | where ($it | str downcase) =~ 'scoop' | first)
          ] | where not ($it | is-empty)
        } catch {
          []
        }
      ''
    else
      "[]";
in
{
  home.persistence."/persist".files = [
    ".config/nushell/history.txt"
    ".config/nushell/history.sqlite3"
    ".config/nushell/history.sqlite3-shm"
    ".config/nushell/history.sqlite3-wal"
  ];

  # fix for wally cli
  home.packages = [
    (pkgs.writeShellScriptBin "sudo-wally" ''
      sudo ${lib.getExe pkgs.wally-cli} "$@"
    '')
  ];

  programs = {
    # carapace = {
    #   enable = true;
    #   enableNushellIntegration = true;
    # };
    # starship.enable = true;
    # starship.enableNushellIntegration = true;
    yazi.enableNushellIntegration = true;
    zoxide.enableNushellIntegration = true;
    direnv.enableNushellIntegration = true;

    nushell = {
      enable = true;
      shellAliases = {
        fg = "job unfreeze";
        imv = "imv-dir";
        lg = "lazygit";
        fz = "fzf --bind 'enter:become(hx {})'";
        cat = "${pkgs.bat}/bin/bat";
        df = "${pkgs.duf}/bin/duf";
        find = "${pkgs.fd}/bin/fd";
        grep = "${pkgs.ripgrep}/bin/rg --no-heading --line-number";
        tree = "${pkgs.eza}/bin/eza --git --icons --tree";
        "7z" = "7zz";
      };
      extraEnv =
        let
          varsToImport = [
            "WINEPREFIX"
            "EDITOR"
            "TERMBROWSER"
            "FILEBROWSER"
            "HOSTNAME"
          ];

          mkNuVar =
            varName:
            lib.optionalString (builtins.hasAttr varName config.home.sessionVariables) ''
              $env.${varName} = "${config.home.sessionVariables.${varName}}"
            '';

          importedVars = lib.concatStringsSep "\n" (map mkNuVar varsToImport);
        in
        # nu
        ''
          ${importedVars}

          $env.EDITOR = "hx"
          $env.VISUAL = "hx"
          $env.HOSTNAME = "${host}"
          $env.FZF_DEFAULT_OPTS_FILE = "/home/${config.home.username}/.config/fzf/colors"

          $env.COLORTERM = "truecolor"
          # $env.STARSHIP_SHELL = "nu"

          $env.config.buffer_editor = "hx"
          $env.config.cursor_shape.vi_insert = "blink_line"
          $env.config.cursor_shape.vi_normal = "blink_block"

          $env.NU_EXPERIMENTAL_OPTIONS = "native-clip"

          # AI key setup
          let gemini_key = ($env.HOME | path join '.ssh/gemini')
          if ($gemini_key | path exists) {
              $env.GEMINI_API_KEY = (open $gemini_key | str trim)
          }
          let steam_key = ($env.HOME | path join '.ssh/steam')
          if ($steam_key | path exists) {
              $env.STEAMGRIDDB = (open $steam_key | str trim)
          }

          $env.NU_LIB_DIRS = ($env.NU_LIB_DIRS |
            split row (char esep) |
            append ($env.HOME | path join "Flake" "home" "common" "programs" "terminal" "nushell")
          )

          let wanted_paths = ${win_block}
          $env.PATH = ($env.PATH | 
            split row (char esep) |
            prepend /home/${config.home.username}/Flake/bin |
            prepend $wanted_paths |
            append /usr/bin/env
          )
                    
        '';

      extraConfig = # nu
        ''
          const NU_LIB_DIRS = [
            '/home/${config.home.username}/Flake/home/common/programs/terminal/nushell'
          ]

          source qmk.nu
          source colors.nu
          source prompt.nu
          source completion.nu
          source keys.nu
          source functions.nu

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
              algorithm: "fuzzy",
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
