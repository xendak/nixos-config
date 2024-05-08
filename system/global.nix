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


  environment.etc."/bluetooth/main.conf".text = ''
    [General]
    ControllerMode=dual
    Enable=Source,Sink,Media,Socket

    [Policy]
    AutoEnable=true
  '';
  # root xdg
  environment.etc."xdg/Xresources".text = ''
    Xcursor.size: 32
    Xcursor.theme: Bibata-Modern-Classic
  '';

  # XDG - PORTAL
  environment.systemPackages = with pkgs; [

    lm_sensors
    agenix
    libsForQt5.qtstyleplugins
    qt5.qtwayland
    qt6.qtwayland
  ];

  # xdg.portal = {
  #   enable = true;
  #   #wlr.enable = true;
  #   #gtkUsePortal = true;
  #   extraPortals = [ inputs.hyprland-portal.packages.${pkgs.system}.xdg-desktop-portal-hyprland ];
  #   configPackages = [ inputs.hyprland.hyprland ];
  # };

  xdg.portal = {
    extraPortals = [ pkgs.inputs.hyprland.xdg-desktop-portal-hyprland ];
    configPackages = [ pkgs.inputs.hyprland.hyprland ];
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
      "/var/lib/bluetooth"
      # "/etc/bluetooth"
    ];
    files = [
      "/etc/adjtime"
      "/etc/machine-id"
      "/etc/ssh/ssh_host_ed25519_key"
      "/etc/ssh/ssh_host_ed25519_key.pub"
      "/etc/ssh/ssa_host_rsa_key"
      "/etc/ssh/ssa_host_rsa_key.pub"
      "/etc/ssh/ssa_host_ecdsa_key"
      "/etc/ssh/ssh_host_ecdsa_key.pub"
      #{ file = "/home/secrets/passwd-root"; parentDirectory = { mode = "0700"; }; }
      #{ file = "/home/secrets/passwd-flakes"; parentDirectory = { mode = "0700"; }; }
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

  system.stateVersion = "24.05";

}
