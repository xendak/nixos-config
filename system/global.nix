{
  config,
  pkgs,
  lib,
  inputs,
  outputs,
  ...
}:
let
  ENGLISH = "en_US.UTF-8";
  JAPANESE = "ja_JP.UTF-8";
  PORTUGUESE = "pt_BR.UTF-8";
in
{
  imports = [
    inputs.impermanence.nixosModules.impermanence
    inputs.home-manager.nixosModules.home-manager
    inputs.agenix.nixosModules.default

    #inputs.hardware.nixosModules.common-gpu-amd
    #inputs.hardware.nixosModules.common-cpu-intel
    #inputs.hardware.nixosModules.common-pc-ssd

    ./extras/postMountFiles.nix
    ./extras/bash.nix
    ./extras/fish.nix
    ./extras/fonts.nix
    ./openssh.nix
    #./extras/openssh.nix
    ./extras/pipewire.nix
    ./extras/quietboot.nix

    ./hardware-configuration.nix
  ];

  nixpkgs = {
    # You can add overlays here
    overlays = [
      # Add overlays your own flake exports (from overlays and pkgs dir):
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages

      inputs.agenix.overlays.default
      # inputs.ragenix.overlays.default

      # Eww systray
      inputs.rust-overlay.overlays.default

      # You can also add overlays exported from other flakes:
      # neovim-nightly-overlay.overlays.default

      # Or define it inline, for example:
      # (final: prev: {
      #   hi = final.hello.overrideAttrs (oldAttrs: {
      #     patches = [ ./change-hello-to-hi.patch ];
      #   });
      # })
    ];
    # Configure your nixpkgs instance
    config = {
      # Disable if you don't want unfree packages
      allowUnfree = true;
      chromium.commandLineArgs = "--gtk-version=4 --enable-features=UseOzonePlatform --ozone-platform=wayland";
    };
  };

  age.secrets.github-token = {
    file = ../secrets/github-token.age;
    owner = "root";
    group = "root";
  };

  age.identityPaths = [ "/persist/etc/ssh/ssh_host_ed25519_key" ];

  nix = {
    # extraOptions = ''access-tokens = github.com=${config.age.secrets.github-token.path}'';
    # extraOptions = ''access-tokens = github.com=${builtins.readFile config.age.secrets.github-token.path}'';
    # This will add each flake input as a registry
    # To make nix3 commands consistent with your flake
    registry = lib.mapAttrs (_: value: { flake = value; }) inputs;

    # This will additionally add your inputs to the system's legacy channels
    # Making legacy nix commands consistent as well, awesome!
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;

    settings = {
      # was getting a warning
      download-buffer-size = 524288000;
      # Enable flakes and new 'nix' command
      experimental-features = "nix-command flakes";
      # Deduplicate and optimize nix store
      auto-optimise-store = true;
      substituters = [
        "https://hyprland.cachix.org"
        "https://ezkea.cachix.org"
        "https://nix-community.cachix.org"
        "https://cache.nixos.org/"
      ];
      trusted-public-keys = [
        "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "ezkea.cachix.org-1:ioBmUbJTZIKsHmWWXPe1FSFbeVe+afhfgqgTSNd34eI="
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      ];
    };
  };

  # Adds non sudo wally cli
  security.sudo = {
    enable = true;
    extraRules = [
      {
        commands = [
          {
            command = "${lib.getExe pkgs.evtest}";
            options = [ "NOPASSWD" ];
          }
          {
            command = "${lib.getExe pkgs.wally-cli}";
            options = [ "NOPASSWD" ];
          }
        ];
        groups = [ "wheel" ];
      }
    ];
  };

  hardware.enableRedistributableFirmware = true;

  # root xdg
  environment.etc."xdg/Xresources".text = ''
    Xcursor.size: 32
    Xcursor.theme: Bibata-Modern-Classic
  '';

  qt = {
    enable = true;
    # do i even need this here?
    platformTheme = "qt5ct";
  };

  environment.variables = {
    EDITOR = "hx";
    SUDO_EDITOR = "vi";
  };

  # XDG - PORTAL
  environment.systemPackages = with pkgs; [
    ripgrep
    fd
    eza
    bat

    helix

    busybox
    lm_sensors
    agenix
    # libsForQt5.qtstyleplugins
    # qt5.qtwayland
    # qt6.qtwayland
    # xdg-desktop-portal-gtk
    # kdePackages.xdg-desktop-portal-kde
    xdg-desktop-portal-wlr
    xdg-desktop-portal-hyprland
    # pkgs.custom-xdg-desktop-portal-termfilechooser
    xdg-desktop-portal-termfilechooser
    pkgs.papirus-icon-theme
    pkgs.papirus-folders
    pkgs.adwaita-icon-theme
  ];

  xdg.portal = {
    enable = true;
    # xdgOpenUsePortal = true;
    config = {
      common = {
        default = [ "gnome" ];
        "org.freedesktop.impl.portal.FileChooser" = [ "termfilechooser" ];
      };
      hyprland = {
        default = [
          "hyprland"
          "gtk"
        ];
        "org.freedesktop.impl.portal.FileChooser" = [ "termfilechooser" ];
        "org.freedesktop.impl.portal.Access" = "gtk";
        "org.freedesktop.impl.portal.Notification" = "gtk";
      };
      niri = {
        default = [
          "gnome"
          "gtk"
        ];
        "org.freedesktop.impl.portal.FileChooser" = [ "termfilechooser" ];
        "org.freedesktop.impl.portal.Access" = "gnome";
        "org.freedesktop.impl.portal.Notification" = "gnome";

      };

    };
    wlr.enable = true;
    extraPortals = [
      pkgs.xdg-desktop-portal-hyprland
      pkgs.xdg-desktop-portal-termfilechooser
      pkgs.xdg-desktop-portal-gtk
      pkgs.xdg-desktop-portal-gnome
      # pkgs.kdePackages.xdg-desktop-portal-kde
      pkgs.xdg-desktop-portal-wlr
    ];
    #configPackages = [ pkgs.inputs.hyprland.hyprland ];
  };

  # Gamemode
  programs.gamemode = {
    enable = true;
    enableRenice = true;
    settings = {
      general = {
        softrealtime = "auto";
        renice = 10;
      };
      custom = {
        start = "notify-send -a 'Gamemode' 'Optimizations activated'";
        end = "notify-send -a 'Gamemode' 'Optimizations deactivated'";
      };
    };
  };

  # Persistence -----------------------------
  programs.fuse.userAllowOther = true;
  programs.dconf.enable = true;
  # environment.enableAllTerminfo = true;
  environment.persistence."/persist" = {
    hideMounts = true;
    directories = [
      "/etc/nixos"
      "/var/lib/systemd"
      "/var/lib/bluetooth"
      "/var/lib/nixos"
      # "/etc/bluetooth"
    ];
    files = [
      # "/etc/adjtime"
      "/etc/machine-id"
      "/etc/ssh/ssh_host_ed25519_key.pub"
      "/etc/ssh/ssa_host_rsa_key.pub"
      "/etc/ssh/ssh_host_ecdsa_key.pub"
    ];
  };

  # Services -----------------------------
  services.dbus.packages = [
    pkgs.gcr
  ];

  services.udev.packages = with pkgs; [ gnome-settings-daemon ];

  services.udisks2.enable = true;
  services.fstrim.enable = true;
  services.speechd.enable = false;

  services.xserver.desktopManager.runXdgAutostartIfNone = true;
  # services.xserver = {
  #   enable = true;
  #   dpi = 96;
  #   desktopManager.gnome.enable = false;
  #   displayManager = {
  #     gdm.enable = false;
  #     sddm.enable = false;
  #     lightdm.enable = false;
  #   };
  # };

  # localtime specific
  time.timeZone = lib.mkDefault "America/Sao_Paulo";
  time.hardwareClockInLocalTime = false; # Lets use proper UTC.
  # services.localtimed.enable = true;
  services.automatic-timezoned.enable = true;
  services.geoclue2.enable = true;
  services.geoclue2.geoProviderUrl = "https://api.beacondb.net/v1/geolocate";

  # locale configs
  i18n.supportedLocales = [
    "en_US.UTF-8/UTF-8"
    "ja_JP.UTF-8/UTF-8"
    "pt_BR.UTF-8/UTF-8"
  ];
  i18n.defaultLocale = lib.mkDefault ENGLISH;
  i18n.extraLocaleSettings = {
    LANG = JAPANESE;
    LC_MESSAGES = ENGLISH;
    LC_IDENTIFICATION = ENGLISH;
    LC_CTYPE = ENGLISH;
    LC_NUMERIC = ENGLISH;
    LC_TIME = lib.mkDefault JAPANESE;
    LC_COLLATE = ENGLISH;
    LC_NAME = ENGLISH;
    LC_MONETARY = lib.mkDefault PORTUGUESE;
    LC_PAPER = ENGLISH;
    LC_ADDRESS = ENGLISH;
    LC_TELEPHONE = ENGLISH;
    LC_MEASUREMENT = ENGLISH;
  };

  # polkit agent
  security.polkit.enable = true;
  security.rtkit.enable = true;
  systemd = {
    user.services.polkit-gnome-authentication-agent-1 = {
      description = "polkit-gnome-authentication-agent-1";
      wantedBy = [ "graphical-session.target" ];
      wants = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
        Restart = "on-failure";
        RestartSec = 1;
        TimeoutStopSec = 10;
      };
    };
  };

  system.stateVersion = lib.mkDefault "25.05";
}
