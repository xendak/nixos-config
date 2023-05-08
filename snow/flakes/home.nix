{ inputs, outputs, lib, config, pkgs, ...}:
{
  imports = [
    #./fonts
    ../common
    #./hyprland
    # ./programs
    # ./games
    ./setup.nix
    ../common/hyprland/waybar/waybar-flakes.nix
    # ./gtk.nix
    # ./qt.nix
    # ./xdg.nix
    # ./font.nix
  ];

  home.packages = with pkgs; [
    (nnn.override { withNerdIcons = true; })
    polkit_gnome
    obs-studio
    mangohud
    mpv
    jq
    unzip
    unrar
    p7zip
    wineWowPackages.waylandFull
    bottom
    xdg-utils
    nautilus
    mkxp-z
    yuzu-updated
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

    # keyboard
    qmk
    wally-cli
  ];

  programs = {
    home-manager.enable = true;
  };

  xdg.mimeApps.enable = true;

  i18n = {
    inputMethod = {
      enabled = "fcitx5";
      # fcitx.engines = with pkgs.fcitx-engines; [ mozc ];
      fcitx5.addons = with pkgs; [
        fcitx5-mozc
        fcitx5-gtk
        libsForQt5.fcitx5-qt
      ];
    };
  };
  
  services.udiskie.enable = true;
  services.playerctld.enable = true;
  systemd.user.startServices = "sd-switch";
  
  # Home --------------------
  home = {
    username = lib.mkDefault "flakes";
    homeDirectory = lib.mkDefault "/home/flakes/";
    stateVersion = lib.mkDefault "23.05";
    persistence = { 
      "/persist/snow/flakes" = {
        directories = [
          "Flake"
          "Downloads"
          "Music"
          "Videos"
          "Programming"
          "Pictures"
          "Documents"
          ".config/fcitx5"
          ".config/OpenRGB"
          # ".nixops"
          # ".local/share/direnv"
          # ".local/share/keyrings"
          ".local/share/Terraria"
          ".local/share/fonts"
          ".local/state/wireplumber"
        ];
        allowOther = true;
      };
    };
  };

  home.sessionPath = [
    "$HOME/Flakes/bin"
  ];

  home.sessionVariables = {
    SCREENSHOT_DIR = "$HOME/Pictures/Screenshots";
    XCURSOR_PATH = "${config.gtk.cursorTheme.package}/share/icons/:$XCURSOR_PATH";
    FULLSCREEN_SAVE_FILE = "$(date +%Y-%m-%d_%M).png";
    AREA_SAVE_FILE = "$(date +%Y-%m-%d_%M)_snip.png";
    AREA_CONFIG_DIR = "Snips";
    NNN_TMPFILE = "$XDG_CONFIG_HOME/.config/nnn/.lastd";
    GTK_THEME = "${config.gtk.theme.name}:dark";
    EDITOR = "nvim";
    TERMINAL = "kitty -1";
    BROWSER = "firefox";
    FILEBROWSER = "nautilus";
    TERMBROWSER = "n";
    MOZ_ENABLE_WAYLAND = 1;
    ANKI_WAYLAND = "1";
    NIXOS_OZONE_WL = "1";
    LIBSEAT_BACKEND = "logind";
    XCURSOR_SIZE = 36;
    QT_QPA_PLATFORM = "wayland;xcb";
    QT_WAYLAND_DISABLE_WINDOWDECORATION = "0";
    WLR_NO_HARDWARE_CURSORS = 1;
    INPUT_METHOD = "fcitx";
    XIM_SERVERS = "fcitx";
    XMODIFIERS = "@im=fcitx";
    XMODIFIER = "@im=fcitx";
    GTK_IM_MODULE = "fcitx";
    QT_IM_MODULE = "fcitx";
    WINEPREFIX = "$HOME/Games/Wine-Prefix";

    # DISPLAY = "";
    # WAYLAND_DISPLAY = "wayland-0";
  };

  # colorscheme = inputs.nix-colors.colorschemes.nord; #TODO:
  monitors = [
    {
      name = "DP-1";
      width = 2560;
      height = 1440;
      refreshRate = 144;
      x = 0;
      y = 0;
      workspace = "1";
      bind = [1 2 3 4 5];
    }
    # {
    #   name = "DP-2";
    #   width = 2560;
    #   height = 1440;
    #   refreshRate = 144;
    #   x = 0;
    #   y = 0;
    #   workspace = "6";
    #   bind = [6 7];
    # }
  ];
}
