{
  lib,
  config,
  pkgs,
  ...
}:
let
  cfg = config.themes;
  palettes = import cfg.palettesPath { inherit lib; };
  templates = import cfg.templatesPath { inherit lib; };

  # Generate themes by combining each palette with each template
  generatedThemes = lib.mapAttrs (
    paletteName: paletteData:
    lib.foldl' lib.recursiveUpdate { } (
      lib.mapAttrsToList (
        templateName: templateFunc:
        templateFunc {
          inherit lib config pkgs;
          paletteSet = paletteData;
        }
      ) templates
    )
  ) palettes;

  themesDerivation = pkgs.runCommand "generated-themes" { } ''
    mkdir -p $out
    ${lib.concatStringsSep "\n" (
      lib.mapAttrsToList (
        themeName: themeFiles:
        lib.concatStringsSep "\n" (
          lib.mapAttrsToList (
            filePath: fileContent:
            let
              fullPath = "$out/${themeName}/${filePath}";
            in
            ''
              mkdir -p "$(dirname "${fullPath}")"
              echo -n ${lib.escapeShellArg fileContent} > "${fullPath}"
            ''
          ) themeFiles
        )
      ) generatedThemes
    )}
  '';

in
{
  options.themes = {
    enable = lib.mkEnableOption "Enable dynamic theming module";
    activeTheme = lib.mkOption {
      type = lib.types.str;
      default = "gorgoroth";
      description = "The name of the currently active theme.";
    };
    targets = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      description = "Mapping of generated file paths to their final destination.";
      example = lib.literalExample ''
        {
          "helix/themes/theme.toml" = "''${config.xdg.configHome}/helix/themes/theme.toml";
          "alacritty/theme.yml" = "''${config.xdg.configHome}/alacritty/theme.yml";
        }
      '';
    };
    palettesPath = lib.mkOption {
      type = lib.types.path;
      default = ./palettes;
      description = "The path to the palettes directory.";
    };
    templatesPath = lib.mkOption {
      type = lib.types.path;
      default = ./templates;
      description = "The path to the templates directory.";
    };
    generated = lib.mkOption {
      type = lib.types.package; # The result is a derivation, which is a type of package
      readOnly = true; # This signals that the user doesn't set it, the module does
      internal = true; # Hides this option from documentation viewers
      description = "The final derivation containing all generated theme files. This is set internally by the module.";
    };
  };
  config = lib.mkIf cfg.enable {
    themes.generated = themesDerivation;
    home.packages = [
      (pkgs.writeShellApplication {
        name = "nix-theme-switcher";
        runtimeInputs = with pkgs; [
          gsettings-desktop-schemas
          jq
        ];
        bashOptions = [
          "errexit"
          "pipefail"
        ];
        text = ''
          set -e

          THEME_NAME="$1"
          ALL_THEMES_DIR="${themesDerivation}"
          SRC_DIR="$ALL_THEMES_DIR/$THEME_NAME"

          if [[ -z "$THEME_NAME" ]]; then
            echo "Nix: $ALL_THEMES_DIR"
            echo "Usage: nix-theme-switcher <theme_name>"
            echo "Available themes:"
            find "$ALL_THEMES_DIR" -maxdepth 1 -type d -printf '%f\n' | tail -n +2 | column
            exit 1
          fi

          if [[ ! -d "$SRC_DIR" ]]; then
            echo "Error: Theme '$THEME_NAME' not found."
            exit 1
          fi

          echo "Switching to theme: $THEME_NAME"

          ${lib.concatStringsSep "\n" (
            lib.mapAttrsToList (
              generatedPath: targetPath:
              if lib.hasInfix "emacs/themes/base16-nix" generatedPath then
                # sh
                ''
                  EDIR="$(dirname "${targetPath}")"
                  TIMESTAMP=$(date +%s)
                  EMACSFILE="base16-''${TIMESTAMP}-theme.el"

                  mkdir -p "''${EDIR}"

                  rm -f "''${EDIR}"/*
                  sleep 0.001

                  echo "  emacs: Generating new theme ''${EDIR}/''${EMACSFILE}..."
                  # cat "$SRC_DIR/${generatedPath}" > "''${EDIR}/''${EMACSFILE}"
                  ln -snf "$SRC_DIR/emacs/themes/base16-nix-theme.el"  "''${EDIR}/''${EMACSFILE}"

                  echo "  emacs: Patching theme name..."
                  sed -i "s,base16-nix,base16-''${TIMESTAMP},g" "''${EDIR}/''${EMACSFILE}"
                ''
              # elif lib.hasInfix "wezterm/colors.lua" generatedPath then
              #  sh
              #   ''
              #   ''
              else
                let
                  cleanGeneratedPath = lib.elemAt (lib.splitString "_clone_" generatedPath) 0;
                in
                # sh
                ''
                  mkdir -p "$(dirname "${targetPath}")"
                  # rm -rf "${targetPath}"
                  # cat "$SRC_DIR/${cleanGeneratedPath}" >"${targetPath}" || true
                  sleep 0.002
                  ln -sfn "$SRC_DIR/${cleanGeneratedPath}" "${targetPath}"
                  echo "Linked ${targetPath}"
                ''
            ) config.themes.targets
          )}

          ## needed to be rm -rf  and cat to force reload of some files... sadly

          themeTypesJson='${builtins.toJSON (lib.mapAttrs (_: p: p.type or "dark") palettes)}'
          THEME_TYPE=$(echo "$themeTypesJson" | jq -r ".[\"$THEME_NAME\"]")

          if [[ "$THEME_TYPE" == "dark" ]]; then
            export GTK_THEME="${config.gtk.theme.name}:dark"
            # gsettings set org.gnome.desktop.interface color-scheme "prefer-dark"
            dconf write /org/gnome/desktop/interface/color-scheme "'prefer-dark'"
            echo "Set GTK preference to dark."
          else
            export GTK_THEME="${config.gtk.theme.name}:light"
            # gsettings set org.gnome.desktop.interface color-scheme "prefer-light"
            dconf write /org/gnome/desktop/interface/color-scheme "'prefer-light'"
            echo "Set GTK preference to light."
          fi

          echo "(load-theme 'base16-$TIMESTAMP t)" > "$EDIR/current-theme.el"
          emacsclient -e "(load-theme 'base16-''${TIMESTAMP} t)" &> /dev/null || true &
          # emacsclient -e "(load-file \"$EDIR/current-theme.el\")" &> /dev/null || true &
          echo "$(date +"%d/%m/%y | %H:%M >")" "Theme switched to $THEME_NAME." >> /tmp/theme-switcher
          pkill -USR1 hx &> /dev/null || true &
          # notify-send "Theme Manager" --expire-time=2000 --app-name="Theme Manager" --icon=preferences-desktop-theme "Theme switched to $THEME_NAME"
        '';
      })
    ];
  };
}
# ln -sfn "$SRC_DIR/${cleanGeneratedPath}" "${targetPath}"

# ${lib.concatStringsSep "\n" (
#   lib.mapAttrsToList (
#     generatedPath: targetPath:
#     let
#       cleanGeneratedPath = lib.elemAt (lib.splitString "_clone_" generatedPath) 0;
#     in
#     ''
#       mkdir -p "$(dirname "${targetPath}")"
#       rm -rf "${targetPath}"
#       sleep 0.005
#       cat "$SRC_DIR/${cleanGeneratedPath}" >"${targetPath}" || true
#       echo "Linked ${targetPath}"
#     ''
#   ) cfg.targets
# )}
