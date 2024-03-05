{ config, pkgs, inputs, outputs, ...}:
{

  home.pointerCursor = {
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Classic";
    size = 36;
    gtk.enable = true;
    x11.enable = true;
  };
  
 # home.sessionVariables = { GTK_USE_PORTAL = "1"; };
  home.sessionVariables = { GTK2_RC_FILES = "${config.xdg.configHome}/gtk-2.0/gtkrc";};
  home.packages = [ 
    pkgs.gtk_engines 
    pkgs.gtk4
  ];

  gtk = {
      enable = true;
      font.name = "Sans 12";

#      theme = {
#        name = "Graphite-Dark-nord";
#        package = (pkgs.graphite-gtk-theme.override { tweaks = ["nord"]; });
#      };

      theme = {
        name = "Orchis-Green-Dark-Compact";
        package = (pkgs.orchis-theme.override { 
          themeVariants = [ "green" ];
          colorVariants = [ "dark" ];
          sizeVariants = [ "compact" ];
          tweaks = [ "compact" ];
          border-radius = 6;
        });
      };

      #iconTheme = {
      #  name = "Tela circle dark";
      #  package = (pkgs.tela-circle-icon-theme.override { colorVariants = ["nord"]; circularFolder = true;});
      #};

      iconTheme = {
        name = "Win10SurDark";
        package = (pkgs.win10sur.override {
          circleFolders = true;
          whitePanel = true;
        });
      };

      gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
      gtk3.extraConfig = {
          gtk-application-prefer-dark-theme = 1;
      };
      gtk4.extraConfig = {
          gtk-application-prefer-dark-theme = 1;
      };
    };
    
    # Set GTK4 properly
    # home.file = {
    #   ".config/gtk-4.0/gtk-dark.css" = {
    #     enable = true;
    #     source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk-dark.css";
    #   };
    #   ".config/gtk-4.0/gtk.css" = {
    #     enable = true;
    #     source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk.css";
    #   };
    #   ".config/gtk-4.0/assets" = {
    #     enable = true;
    #     source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/assets";
    #   };
#   #    ".local/share/icons/default/index.theme".text = ''
#   #      [Icon Theme]
#   #      Inherits=${config.gtk.cursorTheme.name}
#   #    '';
    # };
    
    # prefer dark
    dconf.settings = {
      "org/gnome/desktop/interface" = {
        color-scheme = "prefer-dark";
      };
    };
}

