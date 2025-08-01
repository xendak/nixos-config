{ pkgs, ... }:
{
  home.packages = with pkgs; [
    polkit_gnome
    pavucontrol
    playerctl
    grim
    pipewire
    slurp
    waypipe
    wf-recorder
    wl-clipboard
    wl-mirror
    pfetch
    ydotool
    wl-ocr
  ];
  xdg.mimeApps.enable = true;
  xdg.mimeApps.defaultApplications = {
    "application/x-ms-dos-executable" = [ "wine.desktop" ];
  };

  imports = [
    ./fcitx5.nix
  ];

  home.sessionVariables = {
    NIX_AUTO_RUN = "1";
    MOZ_ENABLE_WAYLAND = 1;
    ANKI_WAYLAND = "1";
    NIXOS_OZONE_WL = "1";
    LIBSEAT_BACKEND = "logind";
    QT_QPA_PLATFORM = "wayland;xcb";
    QT_WAYLAND_DISABLE_WINDOWDECORATION = "0";
    WLR_NO_HARDWARE_CURSORS = 1;
    INPUT_METHOD = "fcitx";
    XIM_SERVERS = "fcitx";
    XMODIFIERS = "@im=fcitx";
    XMODIFIER = "@im=fcitx";
    GTK_IM_MODULE = "fcitx";
    QT_IM_MODULE = "fcitx";
    QT_IM_MODULES = "wayland;fcitx;ibus";
    # XDG_CURRENT_DESKTOP = "GNOME"; # fixing QT no icon bullshit
  };
}
