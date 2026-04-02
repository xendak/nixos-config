{
  lib,
  config,
  pkgs,
  inputs,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix

    ../global.nix

    ../extras/btrfs-optin-persistence.nix
    ../extras/sync-browser.nix
    ../extras/greetd.nix
    ../extras/llm.nix
    ../extras/xendak.nix

    inputs.hardware.nixosModules.common-cpu-intel
    inputs.hardware.nixosModules.common-gpu-amd
    inputs.hardware.nixosModules.common-pc-ssd

    inputs.aagl.nixosModules.default
  ];

  networking.hostName = "Snow";

  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "ahci"
    "nvme"
    "usbhid"
    "usb_storage"
    "sd_mod"
  ];

  boot.kernelParams = [
    "amdgpu.ppfeaturemask=0xffffffff"
    "split_lock_detect=off"
  ];

  boot.kernelModules = [
    "kvm-intel"
    "amdgpu"
    "i2c-dev"
    "i2c-i801"
    "coretemp"
    "v4l2loopback"
  ];
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot = {
    enable = true;
    configurationLimit = 6;
    # to find windows handle.
    # edk2-uefi-shell.enable = true;
    sortKey = "0";

    windows = {
      "11" = {
        title = "Windows 11";
        # it seems this can change? for some reason.
        efiDeviceHandle = "HD1b";
        sortKey = "1";
      };
    };
  };

  boot.extraModulePackages = with config.boot.kernelPackages; [ v4l2loopback ];
  boot.extraModprobeConfig = ''
    options v4l2loopback video_nr=2 card_label="OBS Virtual Camera"
  '';
  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_zen;
    # kernelPackages = pkgs.linuxPackages_latest;
    supportedFilesystems = [
      "btrfs"
      "ntfs"
    ];
  };

  services.greetd = {
    settings = {
      initial_session = {
        command = "niri-session";
        user = "xendak";
      };
    };
  };

  systemd.services.lact = {
    description = "AMDGPU Control Daemon";
    after = [ "multi-user.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.lact}/bin/lact daemon";
    };
    enable = true;
  };

  # genshin
  programs.anime-game-launcher.enable = true;
  # hsr
  programs.honkers-railway-launcher.enable = true;

  environment.systemPackages = [
    pkgs.linuxKernel.packages.linux_zen.v4l2loopback # uncertain if still needed here..?
    pkgs.v4l-utils

    pkgs.lact
    pkgs.openrgb-with-all-plugins
  ];

  boot.initrd.systemd.network.wait-online.enable = false;
  systemd.services.NetworkManager-wait-online.enable = false;
  systemd.network.wait-online.timeout = 0;
  systemd.network.wait-online.enable = false;

  networking.networkmanager.enable = true;
  networking.networkmanager.dns = "none";
  networking.dhcpcd.enable = false;
  networking.useDHCP = false;
  networking.nameservers = [
    "8.8.8.8"
    "8.8.8.4"
    "1.1.1.1"
    "1.0.0.1"
    "208.67.222.222"
    "208.67.220.220"
  ];

  environment.persistence."/persist" = {
    hideMounts = true;
    directories = [
      "/etc/lact"
    ];
  };

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

  nix.settings = {
    secret-key-files = [ config.age.secrets.nix-cache.path ];
    substituters = lib.mkForce [
      "https://hyprland.cachix.org"
      "https://ezkea.cachix.org"
      "https://nix-community.cachix.org"
      "https://cache.nixos.org/"
    ];
    trusted-public-keys = lib.mkForce [
      "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "ezkea.cachix.org-1:ioBmUbJTZIKsHmWWXPe1FSFbeVe+afhfgqgTSNd34eI="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    ];
    sandbox = true;
    # 32gb ram for builds, instead of writing to disk
    max-free = 32 * 1024 * 1024 * 1024;
  };

  services = {
    hardware = {
      openrgb.enable = true;
      openrgb.motherboard = "intel";
    };
    avahi = {
      publish = {
        enable = true;
        addresses = true;
        workstation = true;
      };
    };
  };

  environment.etc."/bluetooth/main.conf".text = ''
    [General]
    ControllerMode=dual
    Enable=Source,Sink,Media,Socket
    DiscoverableTimeout = 0

    [Policy]
    AutoEnable=true
  '';

  hardware = {
    bluetooth.enable = true;
    bluetooth.settings = {
      General = {
        Enable = "Source,Sink,Media,Socket";
      };
    };
    i2c.enable = true;

    cpu.intel.updateMicrocode = true;
    graphics = {
      enable = true;
      enable32Bit = true;
    };
    opentabletdriver.enable = true;
  };

  system.stateVersion = "24.05";
}
