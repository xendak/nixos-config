{
  pkgs,
  inputs,
  config,
  lib,
  ...
}:
let
  quickshell = inputs.quickshell.packages.${pkgs.system}.default;
  mkQuickshellTheme =
    { lib, palette }:
    let
      hex = str: lib.removePrefix "#" str;
    in
    with palette;
    ''
      primary_paletteKeyColor ${hex accent}
      secondary_paletteKeyColor ${hex base0E}
      tertiary_paletteKeyColor ${hex base0B}
      neutral_paletteKeyColor ${hex base04}
      neutral_variant_paletteKeyColor ${hex base03}
      background ${hex base01}
      onBackground ${hex base05}
      surface ${hex base00}
      surfaceDim ${hex base00}
      surfaceBright ${hex base02}
      surfaceContainerLowest ${hex base00}
      surfaceContainerLow ${hex base01}
      surfaceContainer ${hex base02}
      surfaceContainerHigh ${hex base03}
      surfaceContainerHighest ${hex base04}
      onSurface ${hex base05}
      surfaceVariant ${hex base03}
      onSurfaceVariant ${hex base06}
      inverseSurface ${hex base06}
      inverseOnSurface ${hex base00}
      outline ${hex base04}
      outlineVariant ${hex base03}
      shadow 000000
      scrim 000000
      surfaceTint ${hex base0D}
      primary ${hex accent}
      onPrimary ${hex base00}
      primaryContainer ${hex base02}
      onPrimaryContainer ${hex base06}
      inversePrimary ${hex base0C}
      secondary ${hex base0E}
      onSecondary ${hex base00}
      secondaryContainer ${hex base03}
      onSecondaryContainer ${hex base06}
      tertiary ${hex base0B}
      onTertiary ${hex base00}
      tertiaryContainer ${hex base04}
      onTertiaryContainer ${hex base06}
      error ${hex base08}
      onError ${hex base00}
      errorContainer ${hex base02}
      onErrorContainer ${hex base08}
      primaryFixed ${hex accent}
      primaryFixedDim ${hex base0C}
      onPrimaryFixed ${hex base00}
      onPrimaryFixedVariant ${hex base02}
      secondaryFixed ${hex base0E}
      secondaryFixedDim ${hex base0F}
      onSecondaryFixed ${hex base00}
      onSecondaryFixedVariant ${hex base03}
      tertiaryFixed ${hex base0B}
      tertiaryFixedDim ${hex base0A}
      onTertiaryFixed ${hex base00}
      onTertiaryFixedVariant ${hex base02}
      rosewater ${hex base08}
      flamingo ${hex base08}
      pink ${hex base0E}
      mauve ${hex base0E}
      red ${hex base08}
      maroon ${hex base0F}
      peach ${hex base09}
      yellow ${hex base0A}
      green ${hex base0B}
      teal ${hex base0C}
      sky ${hex base0D}
      sapphire ${hex base0D}
      blue ${hex base0D}
      lavender ${hex base0E}
    '';
in
{
  home.packages = [
    quickshell
    pkgs.cava
    pkgs.gtk3
    pkgs.kdePackages.qt5compat
    pkgs.qt6.qt5compat
    pkgs.imagemagick
    pkgs.swww
    pkgs.ddcutil
    pkgs.python3
    pkgs.python3Packages.numpy
    pkgs.python3Packages.pyaudio
    pkgs.app2unit
    pkgs.gnused
  ];

  home.sessionVariables.QML2_IMPORT_PATH = lib.concatStringsSep ":" [
    "${quickshell}/lib/qt-6/qml"
    "${pkgs.kdePackages.qtdeclarative}/lib/qt-6/qml"
    "${pkgs.kdePackages.kirigami.unwrapped}/lib/qt-6/qml"
  ];

  home.file = {
    ".config/themes/quickshell-default.txt".text = mkQuickshellTheme {
      inherit lib;
      palette = config.themes.default.colorScheme.palette;
    };
    ".config/themes/quickshell-dark.txt".text = mkQuickshellTheme {
      inherit lib;
      palette = config.themes.dark.colorScheme.palette;
    };
    ".config/themes/quickshell-light.txt".text = mkQuickshellTheme {
      inherit lib;
      palette = config.themes.light.colorScheme.palette;
    };
  };
}
