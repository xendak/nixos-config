{
  pkgs,
  lib,
  config,
  ...
}:
let
  defaultPalette = config.themes.default.colorScheme.palette;
  darkPalette = config.themes.dark.colorScheme.palette;
  lightPalette = config.themes.light.colorScheme.palette;

  mkQtColorScheme =
    palette:
    let
      mkScheme =
        colors: lib.concatStringsSep ", " (map (color: "#ff" + (lib.removePrefix "#" color)) colors);
    in
    lib.generators.toINI { } {
      ColorScheme = with palette; {
        active_colors = mkScheme [
          base06 # Window text
          base00 # Button background
          base06 # Bright
          base05 # Less bright
          base01 # Dark
          base02 # Less dark
          base06 # Normal text
          base07 # Bright text
          base06 # Button text
          base02 # Normal background
          base00 # Window
          base00 # Shadow
          base0B # Highlight
          base05 # Highlighted text
          base0D # Link
          base0E # Visited link
          base01 # Alternate background
          base01 # Default
          base01 # Tooltip background
          base06 # Tooltip text
          base05 # Placeholder text
        ];

        inactive_colors = mkScheme [
          base04 # Window text
          base00 # Button background
          base05 # Bright
          base04 # Less bright
          base01 # Dark
          base02 # Less dark
          base04 # Normal text
          base05 # Bright text
          base04 # Button text
          base00 # Normal background
          base00 # Window
          base00 # Shadow
          base0B # Highlight
          base05 # Highlighted text
          base0D # Link
          base0E # Visited link
          base00 # Alternate background
          base01 # Default
          base01 # Tooltip background
          base05 # Tooltip text
          base04 # Placeholder text
        ];

        disabled_colors = mkScheme [
          base04 # Window text
          base00 # Button background
          base04 # Bright
          base03 # Less bright
          base00 # Dark
          base01 # Less dark
          base04 # Normal text
          base05 # Bright text
          base04 # Button text
          base00 # Normal background
          base00 # Window
          base00 # Shadow
          base0B # Highlight
          base05 # Highlighted text
          base0D # Link
          base0E # Visited link
          base00 # Alternate background
          base01 # Default
          base01 # Tooltip background
          base04 # Tooltip text
          base03 # Placeholder text
        ];
      };
    };

  hexPairToInt = pair: (builtins.fromTOML "v = 0x${pair}").v;
  hexToRgb =
    hex:
    let
      r = hexPairToInt (lib.substring 1 2 hex);
      g = hexPairToInt (lib.substring 3 2 hex);
      b = hexPairToInt (lib.substring 5 2 hex);
    in
    "${toString r},${toString g},${toString b}";

  mkKdeGlobals =
    { palette, fonts }:
    with palette;
    with fonts;
    ''
      [$Version]
      update_info=kded.upd:kde3.0,mouse_cursor_theme.upd:kde3.4.99,kaccel.upd:kde3.3/r1

      [ColorEffects:Disabled]
      Color=${hexToRgb base03}
      ColorAmount=0
      ColorEffect=0
      ContrastAmount=0.1
      ContrastEffect=2

      [ColorEffects:Inactive]
      ChangeSelectionColor=true
      Color=${hexToRgb base01}
      ColorAmount=0.3
      ColorEffect=2
      ContrastAmount=0.4
      ContrastEffect=2
      Enable=true
      IntensityAmount=0
      IntensityEffect=0

      [Colors:Button]
      BackgroundAlternate=${hexToRgb base02}
      BackgroundNormal=${hexToRgb base01}
      DecorationFocus=${hexToRgb base0D}
      DecorationHover=${hexToRgb base0C}
      ForegroundActive=${hexToRgb base06}
      ForegroundInactive=${hexToRgb base04}
      ForegroundLink=${hexToRgb base0D}
      ForegroundNegative=${hexToRgb base08}
      ForegroundNeutral=${hexToRgb base0A}
      ForegroundNormal=${hexToRgb base05}
      ForegroundPositive=${hexToRgb base0B}
      ForegroundVisited=${hexToRgb base0E}

      [Colors:Selection]
      BackgroundAlternate=${hexToRgb base0C}
      BackgroundNormal=${hexToRgb base0D}
      DecorationFocus=${hexToRgb base0D}
      DecorationHover=${hexToRgb base0C}
      ForegroundActive=${hexToRgb base06}
      ForegroundInactive=${hexToRgb base04}
      ForegroundLink=${hexToRgb base0D}
      ForegroundNegative=${hexToRgb base08}
      ForegroundNeutral=${hexToRgb base0A}
      ForegroundNormal=${hexToRgb base07}
      ForegroundPositive=${hexToRgb base0B}
      ForegroundVisited=${hexToRgb base0E}

      [Colors:Tooltip]
      BackgroundAlternate=${hexToRgb base02}
      BackgroundNormal=${hexToRgb base01}
      DecorationFocus=${hexToRgb base0D}
      DecorationHover=${hexToRgb base0C}
      ForegroundActive=${hexToRgb base06}
      ForegroundInactive=${hexToRgb base04}
      ForegroundLink=${hexToRgb base0D}
      ForegroundNegative=${hexToRgb base08}
      ForegroundNeutral=${hexToRgb base0A}
      ForegroundNormal=${hexToRgb base05}
      ForegroundPositive=${hexToRgb base0B}
      ForegroundVisited=${hexToRgb base0E}

      [Colors:View]
      BackgroundAlternate=${hexToRgb base01}
      BackgroundNormal=${hexToRgb base00}
      DecorationFocus=${hexToRgb base0D}
      DecorationHover=${hexToRgb base0C}
      ForegroundActive=${hexToRgb base06}
      ForegroundInactive=${hexToRgb base04}
      ForegroundLink=${hexToRgb base0D}
      ForegroundNegative=${hexToRgb base08}
      ForegroundNeutral=${hexToRgb base0A}
      ForegroundNormal=${hexToRgb base05}
      ForegroundPositive=${hexToRgb base0B}
      ForegroundVisited=${hexToRgb base0E}

      [Colors:Window]
      BackgroundAlternate=${hexToRgb base02}
      BackgroundNormal=${hexToRgb base01}
      DecorationFocus=${hexToRgb base0D}
      DecorationHover=${hexToRgb base0C}
      ForegroundActive=${hexToRgb base06}
      ForegroundInactive=${hexToRgb base04}
      ForegroundLink=${hexToRgb base0D}
      ForegroundNegative=${hexToRgb base08}
      ForegroundNeutral=${hexToRgb base0A}
      ForegroundNormal=${hexToRgb base05}
      ForegroundPositive=${hexToRgb base0B}
      ForegroundVisited=${hexToRgb base0E}

      [General]
      ColorScheme=${palette.name or "NixGenerated"}
      desktopFont=${regular.family},12,-1,5,50,0,0,0,0,0
      fixed=${monospace.family},12,-1,5,50,0,0,0,0,0
      font=${regular.family},12,-1,5,50,0,0,0,0,0
      menuFont=${regular.family},12,-1,5,50,0,0,0,0,0
      shadeSortColumn=true
      smallestReadableFont=${regular.family},8,-1,5,50,0,0,0,0,0
      taskbarFont=${regular.family},12,-1,5,50,0,0,0,0,0
      toolBarFont=${regular.family},12,-1,5,50,0,0,0,0,0

      [Icons]
      Theme=${config.gtk.iconTheme.name}

      [KDE]
      contrast=7

      [WM]
      activeBackground=${hexToRgb base0D}
      activeBlend=${hexToRgb base0C}
      activeFont=${regular.family},12,-1,5,75,0,0,0,0,0
      activeForeground=${hexToRgb base07}
      inactiveBackground=${hexToRgb base02}
      inactiveBlend=${hexToRgb base03}
      inactiveForeground=${hexToRgb base04}
    '';

  defaultScheme = mkQtColorScheme defaultPalette;
  darkScheme = mkQtColorScheme darkPalette;
  lightScheme = mkQtColorScheme lightPalette;
in
{
  qt = {
    enable = true;
    platformTheme.name = "qtct";
    style.package = with pkgs; [
      adwaita-qt
      adwaita-qt6
    ];
  };
  home.packages = with pkgs; [
    libsForQt5.qt5.qtwayland
    kdePackages.qtwayland
  ];

  home.file = {
    # https://wiki.archlinux.org/title/Dolphin#Mismatched_folder_view_background_colors
    # ".config/themes/kdeglobals-dark".text = ''
    #   [Colors:View]
    #   BackgroundNormal=${darkPalette.base02}
    # '';
    # ".config/themes/kdeglobals-default".text = ''
    #   [Colors:View]
    #   BackgroundNormal=${defaultPalette.base02}
    # '';
    # ".config/themes/kdeglobals-light".text = ''
    #   [Colors:View]
    #   BackgroundNormal=${lightPalette.base02}
    # '';

    ".config/themes/kdeglobals-default".text = mkKdeGlobals {
      palette = config.themes.default.colorScheme.palette;
      fonts = config.fontProfiles;
    };
    ".config/themes/kdeglobals-light".text = mkKdeGlobals {
      palette = config.themes.light.colorScheme.palette;
      fonts = config.fontProfiles;
    };
    ".config/themes/kdeglobals-dark".text = mkKdeGlobals {
      palette = config.themes.dark.colorScheme.palette;
      fonts = config.fontProfiles;
    };

    ".config/qt5ct/colors/default.conf".text = defaultScheme;
    ".config/qt6ct/colors/default.conf".text = defaultScheme;
    ".config/qt5ct/colors/dark.conf".text = darkScheme;
    ".config/qt6ct/colors/dark.conf".text = darkScheme;
    ".config/qt5ct/colors/light.conf".text = lightScheme;
    ".config/qt6ct/colors/light.conf".text = lightScheme;

    # --- Qt5 Theme Files ---
    ".config/themes/qt-default.conf".text = ''
      [Appearance]
      custom_palette=true
      color_scheme_path=${config.home.homeDirectory}/.config/qt5ct/colors/default.conf
      icon_theme=${config.gtk.iconTheme.name}-Dark
      standard_dialogs=gtk3
      style=Adwaita-Dark

      [Fonts]
      fixed="${config.fontProfiles.monospace.family},12,-1,5,50,0,0,0,0,0"
      general="${config.fontProfiles.regular.family},12,-1,5,50,0,0,0,0,0"

      [Interface]
      activate_item_on_single_click=1
      buttonbox_layout=0
      cursor_flash_time=1000
      dialog_buttons_have_icons=1
      double_click_interval=400
      gui_effects=@Invalid()
      keyboard_scheme=2
      menus_have_icons=true
      show_shortcuts_in_context_menus=true
      stylesheets=@Invalid()
      toolbutton_style=4
      underline_shortcut=1
      wheel_scroll_lines=3

      [SettingsWindow]
      geometry="@ByteArray(\x1\xd9\xd0\xcb\0\x3\0\0\0\0\0\0\0\0\0\0\0\0\x4\xd3\0\0\x5=\0\0\0\0\0\0\0\0\0\0\x4\xff\0\0\x5w\0\0\0\0\x2\0\0\0\n\0\0\0\0\0\0\0\0\0\0\0\x4\xd3\0\0\x5=)"

      [Troubleshooting]
      force_raster_widgets=1
      ignored_applications=@Invalid()
    '';

    ".config/themes/qt-dark.conf".text = ''
      [Appearance]
      custom_palette=true
      color_scheme_path=${config.home.homeDirectory}/.config/qt5ct/colors/dark.conf
      icon_theme=${config.gtk.iconTheme.name}-Dark
      standard_dialogs=gtk3
      style=Adwaita-Dark

      [Fonts]
      fixed="${config.fontProfiles.monospace.family},12,-1,5,50,0,0,0,0,0"
      general="${config.fontProfiles.regular.family},12,-1,5,50,0,0,0,0,0"

      [Interface]
      activate_item_on_single_click=1
      buttonbox_layout=0
      cursor_flash_time=1000
      dialog_buttons_have_icons=1
      double_click_interval=400
      gui_effects=@Invalid()
      keyboard_scheme=2
      menus_have_icons=true
      show_shortcuts_in_context_menus=true
      stylesheets=@Invalid()
      toolbutton_style=4
      underline_shortcut=1
      wheel_scroll_lines=3

      [SettingsWindow]
      geometry="@ByteArray(\x1\xd9\xd0\xcb\0\x3\0\0\0\0\0\0\0\0\0\0\0\0\x4\xd3\0\0\x5=\0\0\0\0\0\0\0\0\0\0\x4\xff\0\0\x5w\0\0\0\0\x2\0\0\0\n\0\0\0\0\0\0\0\0\0\0\0\x4\xd3\0\0\x5=)"

      [Troubleshooting]
      force_raster_widgets=1
      ignored_applications=@Invalid()
    '';

    ".config/themes/qt-light.conf".text = ''
      [Appearance]
      custom_palette=true
      color_scheme_path=${config.home.homeDirectory}/.config/qt5ct/colors/light.conf
      icon_theme=${config.gtk.iconTheme.name}
      standard_dialogs=gtk3
      style=Adwaita 

      [Fonts]
      fixed="${config.fontProfiles.monospace.family},12,-1,5,50,0,0,0,0,0"
      general="${config.fontProfiles.regular.family},12,-1,5,50,0,0,0,0,0"

      [Interface]
      activate_item_on_single_click=1
      buttonbox_layout=0
      cursor_flash_time=1000
      dialog_buttons_have_icons=1
      double_click_interval=400
      gui_effects=@Invalid()
      keyboard_scheme=2
      menus_have_icons=true
      show_shortcuts_in_context_menus=true
      stylesheets=@Invalid()
      toolbutton_style=4
      underline_shortcut=1
      wheel_scroll_lines=3

      [SettingsWindow]
      geometry="@ByteArray(\x1\xd9\xd0\xcb\0\x3\0\0\0\0\0\0\0\0\0\0\0\0\x4\xd3\0\0\x5=\0\0\0\0\0\0\0\0\0\0\x4\xff\0\0\x5w\0\0\0\0\x2\0\0\0\n\0\0\0\0\0\0\0\0\0\0\0\x4\xd3\0\0\x5=)"

      [Troubleshooting]
      force_raster_widgets=1
      ignored_applications=@Invalid()
    '';
  };
}
