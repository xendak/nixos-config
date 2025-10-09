{
  paletteSet,
  lib,
  config,
  ...
}:
let
  p = paletteSet.palette;
  f = config.fontProfiles;

  mkQtCtScheme =
    colors: lib.concatStringsSep ", " (map (color: "#ff" + (lib.removePrefix "#" color)) colors);
in
{
  # File 1: The color scheme for qt5ct and qt6ct
  "qt/colors.conf" = lib.generators.toINI { } {
    ColorScheme = {
      active_colors = mkQtCtScheme [
        p.base06
        p.base00
        p.base06
        p.base05
        p.base01
        p.base02
        p.base06
        p.base07
        p.base06
        p.base02
        p.base00
        p.base00
        p.accent
        p.base00
        p.base0D
        p.base0E
        p.base01
        p.base01
        p.base01
        p.base06
        p.base05
      ];
      inactive_colors = mkQtCtScheme [
        p.base04
        p.base00
        p.base05
        p.base04
        p.base01
        p.base02
        p.base04
        p.base05
        p.base04
        p.base00
        p.base00
        p.base00
        p.accent
        p.base00
        p.base0D
        p.base0E
        p.base00
        p.base01
        p.base01
        p.base05
        p.base04
      ];
      disabled_colors = mkQtCtScheme [
        p.base04
        p.base00
        p.base04
        p.base03
        p.base00
        p.base01
        p.base04
        p.base05
        p.base04
        p.base00
        p.base00
        p.base00
        p.accent
        p.base00
        p.base0D
        p.base0E
        p.base00
        p.base01
        p.base01
        p.base04
        p.base03
      ];
    };
  };

  "qt/qt.conf" = ''
    [Appearance]
    custom_palette=true
    color_scheme_path=${config.xdg.configHome}/qt5ct/colors/current.conf
    icon_theme=${
      if paletteSet.type == "dark" then "${config.gtk.iconTheme.name}-Dark" else config.gtk.iconTheme.name
    }
    standard_dialogs=gtk3
    style=${if paletteSet.type == "dark" then "Adwaita-Dark" else "Adwaita"}

    [Fonts]
    fixed="${f.monospace.family},12,-1,5,50,0,0,0,0,0"
    general="${f.regular.family},12,-1,5,50,0,0,0,0,0"

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

  "qt/kdeglobals" = ''
    [$Version]
    update_info=kded.upd:kde3.0,mouse_cursor_theme.upd:kde3.4.99,kaccel.upd:kde3.3/r1

    [ColorEffects:Disabled]
    Color=${p.base03}
    ColorAmount=0
    ColorEffect=0
    ContrastAmount=0.1
    ContrastEffect=2

    [ColorEffects:Inactive]
    ChangeSelectionColor=true
    Color=${p.base01}
    ColorAmount=0.3
    ColorEffect=2
    ContrastAmount=0.4
    ContrastEffect=2
    Enable=true
    IntensityAmount=0
    IntensityEffect=0

    [Colors:Button]
    BackgroundAlternate=${p.base02}
    BackgroundNormal=${p.base01}
    DecorationFocus=${p.base0D}
    DecorationHover=${p.base0C}
    ForegroundActive=${p.base06}
    ForegroundInactive=${p.base04}
    ForegroundLink=${p.base0D}
    ForegroundNegative=${p.base08}
    ForegroundNeutral=${p.base0A}
    ForegroundNormal=${p.base05}
    ForegroundPositive=${p.base0B}
    ForegroundVisited=${p.base0E}

    [Colors:Selection]
    BackgroundAlternate=${p.base0C}
    BackgroundNormal=${p.base0D}
    DecorationFocus=${p.base0D}
    DecorationHover=${p.base0C}
    ForegroundActive=${p.base06}
    ForegroundInactive=${p.base04}
    ForegroundLink=${p.base0D}
    ForegroundNegative=${p.base08}
    ForegroundNeutral=${p.base0A}
    ForegroundNormal=${p.base07}
    ForegroundPositive=${p.base0B}
    ForegroundVisited=${p.base0E}

    [Colors:Tooltip]
    BackgroundAlternate=${p.base02}
    BackgroundNormal=${p.base01}
    DecorationFocus=${p.base0D}
    DecorationHover=${p.base0C}
    ForegroundActive=${p.base06}
    ForegroundInactive=${p.base04}
    ForegroundLink=${p.base0D}
    ForegroundNegative=${p.base08}
    ForegroundNeutral=${p.base0A}
    ForegroundNormal=${p.base05}
    ForegroundPositive=${p.base0B}
    ForegroundVisited=${p.base0E}

    [Colors:View]
    BackgroundAlternate=${p.base01}
    BackgroundNormal=${p.base00}
    DecorationFocus=${p.base0D}
    DecorationHover=${p.base0C}
    ForegroundActive=${p.base06}
    ForegroundInactive=${p.base04}
    ForegroundLink=${p.base0D}
    ForegroundNegative=${p.base08}
    ForegroundNeutral=${p.base0A}
    ForegroundNormal=${p.base05}
    ForegroundPositive=${p.base0B}
    ForegroundVisited=${p.base0E}

    [Colors:Window]
    BackgroundAlternate=${p.base02}
    BackgroundNormal=${p.base01}
    DecorationFocus=${p.base0D}
    DecorationHover=${p.base0C}
    ForegroundActive=${p.base06}
    ForegroundInactive=${p.base04}
    ForegroundLink=${p.base0D}
    ForegroundNegative=${p.base08}
    ForegroundNeutral=${p.base0A}
    ForegroundNormal=${p.base05}
    ForegroundPositive=${p.base0B}
    ForegroundVisited=${p.base0E}

    [General]
    ColorScheme=current
    desktofFont=${f.regular.family},12,-1,5,50,0,0,0,0,0
    fixed=${f.monospace.family},12,-1,5,50,0,0,0,0,0
    font=${f.regular.family},12,-1,5,50,0,0,0,0,0
    menuFont=${f.regular.family},12,-1,5,50,0,0,0,0,0
    shadeSortColumn=true
    smallestReadableFont=${f.regular.family},8,-1,5,50,0,0,0,0,0
    taskbarFont=${f.regular.family},12,-1,5,50,0,0,0,0,0
    toolBarFont=${f.regular.family},12,-1,5,50,0,0,0,0,0

    [Icons]
    Theme=${
      if paletteSet.type == "dark" then "${config.gtk.iconTheme.name}-Dark" else config.gtk.iconTheme.name
    }

    [KDE]
    contrast=7

    [WM]
    activeBackground=${p.base0D}
    activeBlend=${p.base0C}
    activeFont=${f.regular.family},12,-1,5,75,0,0,0,0,0
    activeForeground=${p.base07}
    inactiveBackground=${p.base02}
    inactiveBlend=${p.base03}
    inactiveForeground=${p.base04}
  '';
}
