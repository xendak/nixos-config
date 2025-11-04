{
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    (lutris.override {
      extraPkgs =
        pkgs: with pkgs; [
          vulkan-loader
          vulkan-tools
          curl
          nghttp2
          xdelta
          mangohud
          winetricks
          gamescope
        ];
    })
  ];

  # fix for wrong portal
  home.file = {
    ".local/share/applications/net.lutris.Lutris.desktop".source =
      pkgs.writeText "net.lutris.Lutris.desktop"
        #ini
        ''
          [Desktop Entry]
          Name=Lutris
          StartupWMClass=Lutris
          Comment=Video Game Preservation Platform
          Categories=Game;
          Keywords=gaming;wine;emulator;
          Exec=env GTK_USE_PORTAL=1 lutris %U
          Icon=net.lutris.Lutris
          Terminal=false
          Type=Application
          StartupNotify=true
          MimeType=x-scheme-handler/lutris;
          X-GNOME-UsesNotifications=true
        '';
  };

  home.persistence = {
    "/persist" = {
      directories = [
        "Games/Lutris-Prefix"
        ".config/lutris"
        ".local/share/lutris"
        ".local/cache/lutris"
      ];
    };
  };
}
