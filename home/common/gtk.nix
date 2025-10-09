{
  config,
  pkgs,
  lib,
  ...
}:
# let
# inherit
# (inputs.nix-colors.lib-contrib {inherit pkgs;})
# gtkThemeFromScheme
# ;
# in
let
  cursorTheme = {
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Classic";
    size = 32;
  };

  wineCursorReg = pkgs.writeText "wine-cursor-theme.reg" ''
    Windows Registry Editor Version 5.00

    [HKEY_CURRENT_USER\Control Panel\Cursors]
    "Arrow"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/left_ptr"
    "AppStarting"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/watch"
    "Hand"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/hand2"
    "Help"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/question_arrow"
    "No"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/crossed_circle"
    "NWPen"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/pencil"
    "SizeAll"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/sizing"
    "SizeNESW"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/nesw-resize"
    "SizeNS"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/ns-resize"
    "SizeNWSE"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/nwse-resize"
    "SizeWE"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/we-resize"
    "UpArrow"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/up_arrow"
    "Wait"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/watch"
    "Crosshair"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/cross"
    "IBeam"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/xterm"
    "Link"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/hand2"
  '';
in
{
  home.pointerCursor = {
    inherit (cursorTheme) package name size;
    gtk.enable = true;
    x11.enable = true;
  };

  # home.sessionVariables = { GTK_USE_PORTAL = "1"; };
  home.sessionVariables = {
    GTK2_RC_FILES = lib.mkDefault "${config.xdg.configHome}/gtk-2.0/gtkrc";
    XCURSOR_SIZE = lib.mkForce (builtins.toString cursorTheme.size);
    XCURSOR_THEME = lib.mkDefault cursorTheme.name;
    XCURSOR_PATH = lib.mkDefault "${config.gtk.cursorTheme.package}/share/icons/:$XCURSOR_PATH";
  };

  home.packages = [
    (pkgs.writeShellScriptBin "writeWriteCursorConfig" ''
      if [ "$(${pkgs.wine}/bin/wine reg query 'HKEY_CURRENT_USER\Control Panel\Cursors' /v Arrow 2> /dev/null | grep -c '${cursorTheme.name}')" -eq 0 ]; then
        echo "Applying Wine cursor theme..."
        ${pkgs.wine}/bin/regedit ${wineCursorReg}
      fi
    '')

    pkgs.gtk_engines
    pkgs.gtk4
    pkgs.adw-gtk3
    pkgs.catppuccin-gtk
    pkgs.adwaita-qt6
    pkgs.adwaita-qt
    pkgs.kdePackages.breeze
    pkgs.kdePackages.breeze-gtk
    pkgs.papirus-icon-theme
    pkgs.papirus-folders
  ];

  gtk = {
    enable = true;
    font.name = "Sofia Pro 12";

    theme = {
      name = "adw-gtk3";
      package = pkgs.adw-gtk3;
    };
    # theme = {
    #   name = "${config.colorScheme.slug}";
    #   package = gtkThemeFromScheme {scheme = config.colorScheme;};
    # };
    #
    # iconTheme = {
    #   name = "Gruvbox-Plus-Dark";
    #   package = pkgs.gruvbox-plus-icons;
    # };

    # iconTheme = {
    #   name = "Colloid-Dark";
    #   package = pkgs.colloid-icon-theme;
    # };

    #      theme = {
    #        name = "Graphite-Dark-nord";
    #        package = (pkgs.graphite-gtk-theme.override { tweaks = ["nord"]; });
    #      };

    # theme = {
    #   name = "Orchis-Green-Dark-Compact";
    #   package = (pkgs.orchis-theme.override {
    #     themeVariants = [ "green" ];
    #     colorVariants = [ "dark" ];
    #     sizeVariants = [ "compact" ];
    #     tweaks = [ "compact" ];
    #     border-radius = 6;
    #   });
    # };

    # iconTheme = {
    #   name = "Tela circle dark";
    #   package = pkgs.tela-circle-icon-theme.override {
    #     colorVariants = ["nord"];
    #     circularFolder = true;
    #   };
    # };
    # iconTheme = {
    #   name = "WhiteSur";
    #   package = (
    #     pkgs.whitesur-icon-theme.override {
    #       boldPanelIcons = true;
    #       themeVariants = [ "green" ];
    #       alternativeIcons = true;
    #     }
    #   );
    # };
    iconTheme = {
      name = "Papirus";
      package = pkgs.papirus-icon-theme;
    };

    # iconTheme = {
    #   name = "Win10SurDark";
    #   package = (pkgs.win10sur.override {
    #     circleFolders = true;
    #     whitePanel = true;
    #   });
    # };

    gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
  };
}
