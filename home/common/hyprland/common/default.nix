{ pkgs, lib, outputs, ... }:
{
  imports = [
    ./font.nix
    ./kitty.nix
    ./fish.nix
    ./waybar.nix
    #./mako.nix
    ./zathura.nix
    ./hyprpaper.nix

  ];


  #i18n.defaultLocale = "en_US.UTF-8";
  i18n = {
    inputMethod = {
      enabled = "fcitx5";
      fcitx.engines = with pkgs.fcitx-engines; [ mozc ];
      fcitx5.addons = with pkgs; [
        fcitx5-mozc
        fcitx5-gtk
        libsForQt5.fcitx5-qt
      ];
    };
  };

  xdg.mimeApps.enable = true;
  home.packages = with pkgs; [
    blender
    deluge
    pavucontrol
    playerctl

    hyprpaper
    grim
    pipewire
    slurp
    waypipe
    wf-recorder
    wl-clipboard
    wl-mirror
    pfetch
    ydotool
  ];

  # SESSION VAR
  home.sessionVariables = {
    MOZ_ENABLE_WAYLAND = 1;
    ANKI_WAYLAND = "1";
    NIXOS_OZONE_WL = "1";
    LIBSEAT_BACKEND = "logind";
    XCURSOR_SIZE = 36;
    QT_QPA_PLATFORM = "wayland;xcb";
    QT_WAYLAND_DISABLE_WINDOWDECORATION = "0";
    INPUT_METHOD = "fcitx";
    XIM_SERVERS = "fcitx";
    XMODIFIERS = "@im=fcitx";
    XMODIFIER = "@im=fcitx";
    GTK_IM_MODULE = "fcitx";
    QT_IM_MODULE = "fcitx";
    WINEPREFIX = "$HOME/Games/Wine-Prefix";
  };
  services.playerctld.enable = true;

}


   
