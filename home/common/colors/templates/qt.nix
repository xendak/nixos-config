{
  paletteSet,
  lib,
  config,
  ...
}:
let
  m = paletteSet.palette;
  f = config.fontProfiles;

  mkQtCtScheme =
    colors: lib.concatStringsSep ", " (map (color: "#ff" + (lib.removePrefix "#" color)) colors);
in
{
  # TODO: find wtf is the highlight/hover
  #   WindowText / ButtonText / Text  → on_surface
  #   Window / Button / Base          → surface* containers
  #   Highlight                       → primary
  #   HighlightedText                 → on_primary
  #   Link / LinkVisited              → secondary / on_secondary_container
  #   BrightText (error signal)       → error
  # Order: WindowText, Window, Light, Midlight, Dark, Mid, Text,
  #        BrightText, ButtonText, Base, Window(again), Shadow,
  #        Highlight, HighlightedText, Link, LinkVisited,
  #        AlternateBase, NoRole, ToolTipBase, ToolTipText, PlaceholderText
  "qt/colors.conf" = lib.generators.toINI { } {
    ColorScheme = {
      active_colors = mkQtCtScheme [
        m.on_surface # WindowText
        m.surface_container # Window
        m.surface_container_high # Light
        m.surface_container # Midlight
        m.surface_container_low # Dark
        m.surface_container # Mid
        m.on_surface # Text
        m.primary # BrightText
        m.on_surface # ButtonText
        m.bg # Base
        m.surface_container_low # Window (alt)
        m.surface_container_low # Shadow
        m.primary # Highlight
        m.on_primary # HighlightedText
        m.secondary # Link
        m.tertiary # LinkVisited
        m.surface_container_low # AlternateBase
        m.surface_container # NoRole
        m.surface_container_high # ToolTipBase
        m.on_surface # ToolTipText
        m.outline # PlaceholderText
      ];
      inactive_colors = mkQtCtScheme [
        m.on_surface_variant # WindowText
        m.surface_container_low # Window
        m.surface_container # Light
        m.surface_container_low # Midlight
        m.background # Dark
        m.outline_variant # Mid
        m.on_surface_variant # Text
        m.error # BrightText
        m.on_surface_variant # ButtonText
        m.background # Base
        m.background # Window (alt)
        m.background # Shadow
        m.primary # Highlight (keep for selection)
        m.on_primary # HighlightedText
        m.secondary # Link
        m.on_secondary_container # LinkVisited
        m.background # AlternateBase
        m.surface_container_low # NoRole
        m.surface_container # ToolTipBase
        m.on_surface_variant # ToolTipText
        m.outline # PlaceholderText
      ];
      disabled_colors = mkQtCtScheme [
        m.outline # WindowText
        m.surface_container_low # Window
        m.surface_container # Light
        m.surface_container_low # Midlight
        m.background # Dark
        m.outline_variant # Mid
        m.outline # Text
        m.error # BrightText
        m.outline # ButtonText
        m.background # Base
        m.background # Window (alt)
        m.background # Shadow
        m.primary_container # Highlight (dimmed)
        m.on_primary_container # HighlightedText
        m.secondary # Link
        m.on_secondary_container # LinkVisited
        m.background # AlternateBase
        m.surface_container_low # NoRole
        m.surface_container # ToolTipBase
        m.outline # ToolTipText
        m.outline_variant # PlaceholderText
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

  "qt/current.colors" =
    # ini
    ''
      [General]
      Name=current
      Description=Generated Theme
      Author=xendak

      [Background:Normal]
      Color=${m.bg}

      [Foreground:Normal]
      Color=${m.fg}

      [Colors:Window]
      BackgroundAlternate=${m.surface_container}
      BackgroundNormal=${m.bg}
      ForegroundActive=${m.primary}
      ForegroundInactive=${m.fg}
      ForegroundNormal=${m.fg}

      [Colors:View]
      BackgroundAlternate=${m.surface_container}
      BackgroundNormal=${m.bg}
      DecorationFocus=${m.primary}
      DecorationHover=${m.secondary}
      ForegroundActive=${m.primary}
      ForegroundInactive=${m.fg}
      ForegroundNormal=${m.fg}

      [Colors:Button]
      BackgroundAlternate=${m.surface_container_low}
      BackgroundNormal=${m.surface_container}
      ForegroundNormal=${m.fg}

      [Colors:Selection]
      BackgroundAlternate=${m.secondary}
      BackgroundNormal=${m.primary}
      DecorationFocus=${m.primary}
      DecorationHover=${m.primary}
      ForegroundActive=${m.on_primary}
      ForegroundInactive=${m.on_surface_variant}
      ForegroundLink=${m.secondary}
      ForegroundNegative=${m.error}
      ForegroundNeutral=${m.tertiary}
      ForegroundNormal=${m.on_primary}
      ForegroundPositive=${m.primary}
      ForegroundVisited=${m.on_secondary_container}

      [Colors:Tooltip]
      BackgroundNormal=${m.surface_container_low}
      ForegroundNormal=${m.fg}

      [WM]
      activeBackground=${m.primary}
      activeForeground=${m.bg}
      inactiveBackground=${m.surface_container}
      inactiveForeground=${m.fg}
    '';

  "qt/kdeglobals" = # ini
    ''
      [$Version]
      update_info=kded.upd:kde3.0,mouse_cursor_theme.upd:kde3.4.99,kaccel.upd:kde3.3/r1

      [ColorEffects:Disabled]
      Color=${m.outline}
      ColorAmount=0
      ColorEffect=0
      ContrastAmount=0.1
      ContrastEffect=2

      [ColorEffects:Inactive]
      ChangeSelectionColor=false
      Color=${m.surface_container_low}
      ColorAmount=0.3
      ColorEffect=2
      ContrastAmount=0.4
      ContrastEffect=2
      Enable=true
      IntensityAmount=0
      IntensityEffect=0

      [Colors:Button]
      BackgroundAlternate=${m.surface_container_high}
      BackgroundNormal=${m.surface_container}
      DecorationFocus=${m.primary}
      DecorationHover=${m.primary}
      ForegroundActive=${m.on_surface}
      ForegroundInactive=${m.on_surface_variant}
      ForegroundLink=${m.secondary}
      ForegroundNegative=${m.error}
      ForegroundNeutral=${m.tertiary}
      ForegroundNormal=${m.on_surface}
      ForegroundPositive=${m.primary}
      ForegroundVisited=${m.on_secondary_container}

      [Colors:Selection]
      BackgroundAlternate=${m.secondary}
      BackgroundNormal=${m.primary}
      DecorationFocus=${m.primary}
      DecorationHover=${m.primary}
      ForegroundActive=${m.on_primary}
      ForegroundInactive=${m.on_surface_variant}
      ForegroundLink=${m.secondary}
      ForegroundNegative=${m.error}
      ForegroundNeutral=${m.tertiary}
      ForegroundNormal=${m.on_primary}
      ForegroundPositive=${m.primary}
      ForegroundVisited=${m.on_secondary_container}

      [Colors:Tooltip]
      BackgroundAlternate=${m.surface_container_high}
      BackgroundNormal=${m.surface_container}
      DecorationFocus=${m.primary}
      DecorationHover=${m.primary}
      ForegroundActive=${m.on_surface}
      ForegroundInactive=${m.on_surface_variant}
      ForegroundLink=${m.secondary}
      ForegroundNegative=${m.error}
      ForegroundNeutral=${m.tertiary}
      ForegroundNormal=${m.on_surface}
      ForegroundPositive=${m.primary}
      ForegroundVisited=${m.on_secondary_container}

      [Colors:View]
      BackgroundAlternate=${m.surface_container_high}
      BackgroundNormal=${m.surface}
      DecorationFocus=${m.primary}
      DecorationHover=${m.primary}
      ForegroundActive=${m.on_surface}
      ForegroundInactive=${m.on_surface_variant}
      ForegroundLink=${m.secondary}
      ForegroundNegative=${m.error}
      ForegroundNeutral=${m.on_primary}
      ForegroundNormal=${m.on_surface}
      ForegroundPositive=${m.on_primary}
      ForegroundVisited=${m.on_secondary_container}

      [Colors:Window]
      BackgroundAlternate=${m.surface_container}
      BackgroundNormal=${m.surface_container_low}
      DecorationFocus=${m.primary}
      DecorationHover=${m.primary}
      ForegroundActive=${m.on_surface}
      ForegroundInactive=${m.on_surface_variant}
      ForegroundLink=${m.secondary}
      ForegroundNegative=${m.error}
      ForegroundNeutral=${m.on_primary}
      ForegroundNormal=${m.on_surface}
      ForegroundPositive=${m.on_primary}
      ForegroundVisited=${m.on_secondary_container}

      [General]
      ColorScheme=current
      desktopFont=${f.regular.family},12,-1,5,50,0,0,0,0,0
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
      activeBackground=${m.primary}
      activeBlend=${m.secondary}
      activeFont=${f.regular.family},12,-1,5,75,0,0,0,0,0
      activeForeground=${m.on_primary}
      inactiveBackground=${m.surface_container}
      inactiveBlend=${m.outline_variant}
      inactiveForeground=${m.on_surface_variant}
    '';
}
