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

  # -----------------------------------------------------------------
  # Helper Scripts
  # -----------------------------------------------------------------
  vm-init = pkgs.writeShellScriptBin "vm-init" ''
    set -e
    echo "=> Creating mount point..."
    sudo mkdir -p /root/host
    echo "=> Mounting 9p share..."
    sudo mount -t 9p -o trans=virtio,version=9p2000.L finixcfg /root/host
    echo "=> Done! mounted at /root/host"

    echo "sudo nix --experimental-features "nix-command flakes" run github:nix-community/disko/latest -- --mode destroy,format,mount --flake /root/host#vm"
    echo "sudo nix --experimental-features "nix-command flakes" run github:nix-community/disko/latest -- --mode mount --flake /root/host#vm"
  '';
in
{
  # -----------------------------------------------------------------
  # Imports (Stripped of hardware-configuration, impermanence, & agenix)
  # -----------------------------------------------------------------
  imports = [
    inputs.home-manager.nixosModules.home-manager

    ./extras/bash.nix
    ./extras/fish.nix
    ./extras/fonts.nix
    ./extras/pipewire.nix
    ./openssh.nix
  ];

  # -----------------------------------------------------------------
  # ISO & Kernel Boot Specifics
  # -----------------------------------------------------------------
  system.stateVersion = lib.mkForce "26.11";
  nixpkgs.hostPlatform = "x86_64-linux";
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelParams = [
    "nomodeset"
    "vga=normal"
  ];

  console = {
    earlySetup = true;
    font = "ter-u28n";
    packages = with pkgs; [ terminus_font ];
  };

  # Prevent ZFS build breakage on latest kernel
  boot.supportedFilesystems = lib.mkForce [
    "btrfs"
    "vfat"
    "ext4"
    "xfs"
  ];

  # -----------------------------------------------------------------
  # Nix & Flakes Configuration
  # -----------------------------------------------------------------
  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages
      inputs.rust-overlay.overlays.default
    ];
    config = {
      allowUnfree = true;
      chromium.commandLineArgs = "--gtk-version=4 --enable-features=UseOzonePlatform --ozone-platform=wayland";
    };
  };

  nix = {
    registry = lib.mapAttrs (_: value: { flake = value; }) inputs;
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;
    settings = {
      trusted-users = [
        "root"
        "@wheel"
      ];
      allowed-users = [
        "root"
        "@wheel"
      ];
      download-buffer-size = 524288000;
      experimental-features = "nix-command flakes";
      auto-optimise-store = true;
    };
  };

  # -----------------------------------------------------------------
  # Users & SSH
  # -----------------------------------------------------------------
  services.openssh.settings.PermitRootLogin = lib.mkForce "yes";

  users.users.root.openssh.authorizedKeys.keys = [
    (builtins.readFile ../home/common/ssh/id_ed25519.pub)
    (builtins.readFile ./Snow/ssh_host_ed25519_key.pub)
    (builtins.readFile ./Dew/ssh_host_ed25519_key.pub)
    (builtins.readFile ./Rain/ssh_host_ed25519_key.pub)
  ];

  users.users.xendak = {
    isNormalUser = true;
    group = "users";
    shell = pkgs.nushell;
    extraGroups = [
      "wheel"
      "networkmanager"
    ];
  };

  programs.ssh.knownHostsFiles = [
    ../home/common/ssh/known_hosts
  ];

  # -----------------------------------------------------------------
  # System Essentials, Locale & Security
  # -----------------------------------------------------------------
  networking.networkmanager.enable = true;
  hardware.enableRedistributableFirmware = true;

  time.timeZone = lib.mkDefault "America/Sao_Paulo";
  time.hardwareClockInLocalTime = false;
  services.automatic-timezoned.enable = true;
  services.geoclue2.enable = true;
  services.geoclue2.geoProviderUrl = "https://api.beacondb.net/v1/geolocate";
  location.provider = "geoclue2";

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

  security = {
    polkit.enable = true;
    rtkit.enable = true;
    sudo = {
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
  };

  # -----------------------------------------------------------------
  # Packages & Environment
  # -----------------------------------------------------------------
  environment.variables = {
    EDITOR = "hx";
    SUDO_EDITOR = "hx";
  };

  environment.systemPackages = [
    pkgs.git
    pkgs.helix
    pkgs.openssh
    pkgs.neovim
    pkgs.ntfs3g
    pkgs.i2c-tools
    pkgs.networkmanagerapplet
    pkgs.yazi
    pkgs.sshfs
    pkgs.nnn
    pkgs.eza
    pkgs.lazygit
    pkgs.fzf
    pkgs.bat
    pkgs.duf
    pkgs.fd
    pkgs.rich-cli
    pkgs.disko
    pkgs.ov
    pkgs.btop
    pkgs.htop
    pkgs.kakoune
    pkgs.busybox
    pkgs.lm_sensors
    vm-init
  ];
}
