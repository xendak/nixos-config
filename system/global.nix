{ config, pkgs, lib, inputs, outputs, ... }:
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

    ./extras/fish.nix
    ./extras/fonts.nix
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

  nix = {
    # This will add each flake input as a registry
    # To make nix3 commands consistent with your flake
    registry = lib.mapAttrs (_: value: { flake = value; }) inputs;

    # This will additionally add your inputs to the system's legacy channels
    # Making legacy nix commands consistent as well, awesome!
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;

    settings = {
      # Enable flakes and new 'nix' command
      experimental-features = "nix-command flakes";
      # Deduplicate and optimize nix store
      auto-optimise-store = true;
      substituters = [
        "https://hyprland.cachix.org"
        "https://ezkea.cachix.org"
      ];
      trusted-public-keys = [
        "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
        "ezkea.cachix.org-1:ioBmUbJTZIKsHmWWXPe1FSFbeVe+afhfgqgTSNd34eI="
      ];
 
    };
  };

  hardware.enableRedistributableFirmware = true;


  # root xdg
  environment.etc."xdg/Xresources".text = ''
    Xcursor.size: 36
    Xcursor.theme: Bibata-Modern-Classic
  '';
  # prefer dark

  # root xdg
  # source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk-dark.css";
  # environment.etc = {
  #   "xdg/Xresources".source = "${config.home.homeDirectory}/.Xresources";
  #   "xdg/gtk-2.0/gtkrc".source = "${config.xdg.configHome}/gtk-2.0/gtkrc";
  #   "xdg/gtk-3.0".source = "${config.xdg.configHome}/gtk-3.0";
  #   "xdg/gtk-4.0".source = "${config.xdg.configHome}/gtk-4.0";
  # };


  # XDG - PORTAL
  environment.systemPackages = with pkgs; [

    lm_sensors
    agenix
    libsForQt5.qtstyleplugins
    qt5.qtwayland
    qt6.qtwayland
  ];

  xdg.portal = {
    enable = true;
    #wlr.enable = true;
    #gtkUsePortal = true;
    extraPortals = [ inputs.hyprland-portal.packages.${pkgs.system}.xdg-desktop-portal-hyprland ];

  };

  # User & Host -----------------------------
  users = {
    mutableUsers = false;
    users.flakes = {
      isNormalUser = true;
      shell = pkgs.fish;
      extraGroups = [ "audio" "video" "input" "wheel" ];
      #password = "1";
      passwordFile = "/persist/snow/secrets/passwd-flakes";
      packages = [ pkgs.home-manager ];
    };
    users.root = {
      passwordFile = "/persist/snow/secrets/passwd-root";
    };
  };

  home-manager = {
    users.flakes = import ../snow/flakes/home.nix;
    useUserPackages = true;
    extraSpecialArgs = { inherit inputs outputs; };
  };

  networking.hostName = "Snow";
  networking.nameservers = [ "8.8.8.8" "8.8.4.4" ];

  # GENSHIN PATCH ---------------------------
  networking.hosts = {
    "0.0.0.0" = [
      "overseauspider.yuanshen.com"
      "log-upload-os.hoyoverse.com"
      "log-upload-os.mihoyo.com"

      "public-data-api.mihoyo.com"
      "sg-public-data-api.hoyoverse.com"

      "log-upload.mihoyo.com"
      "devlog-upload.mihoyo.com"
      "uspider.yuanshen.com"
      "sg-public-data-api.hoyoverse.com"

      "prd-lender.cdp.internal.unity3d.com"
      "thind-prd-knob.data.ie.unity3d.com"
      "thind-gke-usc.prd.data.corp.unity3d.com"
      "cdp.cloud.unity3d.com"
      "remote-config-proxy-prd.uca.cloud.unity3d.com"
    ];
  };

  # Persistence -----------------------------
  programs.fuse.userAllowOther = true;
  programs.dconf.enable = true;
  environment.enableAllTerminfo = true;
  environment.persistence."/persist" = {
    hideMounts = true;
    directories = [
      "/etc/nixos"
      "/var/lib/systemd"
    ];
    files = [
      "/etc/adjtime"
      "/etc/machine-id"
      "/etc/ssh/ssh_host_ed25519_key"
      "/etc/ssh/ssa_host_rsa_key"
      #{ file = "/snow/secrets/passwd-root"; parentDirectory = { mode = "0700"; }; }
      #{ file = "/snow/secrets/passwd-flakes"; parentDirectory = { mode = "0700"; }; }
    ];
  };

  # Services -----------------------------
  services.dbus.packages = [ pkgs.gcr ];
  services.udev.packages = with pkgs; [ gnome.gnome-settings-daemon ];

  services.udisks2.enable = true;
  services.fstrim.enable = true;
  services.geoclue2.enable = true;

  # services.xserver.desktopManager.runXdgAutostartIfNone = true;
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

  services.openssh = {
    enable = true;
    # Forbid root login through SSH.
    settings.PermitRootLogin = "no";
    # Use keys only. Remove if you want to SSH using password (not recommended)
    settings.PasswordAuthentication = false;
  };

  # localtime specific
  time.timeZone = "America/Sao_Paulo";
  time.hardwareClockInLocalTime = true;
  services.localtimed.enable = true;


  # locale configs
  i18n.supportedLocales = [ "en_US.UTF-8/UTF-8" "ja_JP.UTF-8/UTF-8" "pt_BR.UTF-8/UTF-8" ];
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

  system.stateVersion = "23.05";

}
