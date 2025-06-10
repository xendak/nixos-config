{
  pkgs,
  config,
  ...
}:
{
  xdg.configFile."qt5ct/qt5ct.conf".text = ''
    [Appearance]
    custom_palette=false
    icon_theme=${config.gtk.iconTheme.name}
    standard_dialogs=gtk3
    style=kvantum-dark

    [Fonts]
    fixed="Sofia Pro,12,-1,5,50,0,0,0,0,0"
    general="Sofia Pro,12,-1,5,50,0,0,0,0,0"

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

  xdg.configFile."qt6ct/qt6ct.conf".text = config.xdg.configFile."qt5ct/qt5ct.conf".text;

  # home.sessionVariables = {
  #   QT_STYLE_OVERRIDE = "kvantum";
  # };

  # try this to run when icons boom. nv QT_QPA_PLATFORMTHEME=qt5ct QT_STYLE_OVERRIDE=kvantum dolphin
  qt = {
    enable = true;
    platformTheme.name = "qtct";
    style.name = "kvantum";
    style.package = with pkgs; [
      kdePackages.qtstyleplugin-kvantum
      # libsForQt5.qtstyleplugin-kvantum
      qt6Packages.qtstyleplugin-kvantum
    ];
  };
}
