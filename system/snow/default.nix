# my main desktop
{ config, lib, pkgs, inputs, outputs, ...}: {
  imports = [
    ../global.nix
    ./btrfs-optin-persistence.nix
    ./hardware-configuration.nix

    inputs.hardware.nixosModules.common-cpu-intel
    inputs.hardware.nixosModules.common-gpu-amd
    inputs.hardware.nixosModules.common-pc-ssd

    inputs.aagl.nixosModules.default

  ];


  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod" ];
  boot.kernelModules = [ "kvm-intel" "amdgpu" "i2c-dev" "i2c-i801" ];
  boot.loader.systemd-boot.enable = true;
  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_zen;
    # kernelPackages = pkgs.linuxPackages_xanmod_latest;
    supportedFilesystems = [ "btrfs" "ntfs" ];
  };

  # genshin
  programs.anime-game-launcher.enable = true;
  programs.honkers-railway-launcher.enable = true;

  # NTFS-3G for Windows Partititions
  environment.systemPackages = [
    pkgs.ntfs3g
    pkgs.openrgb-with-all-plugins
    pkgs.i2c-tools
  ];

  # User & Host -----------------------------
  users = {
    mutableUsers = false;
    users.root = {
      hashedPasswordFile = "/persist/home/secrets/passwd-root";
    };
  };
  
  networking.useDHCP = lib.mkDefault true;
  networking.nameservers = [ "8.8.8.8" "8.8.4.4" ];
  home = "Snow";

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


  users = {
    users.flakes = {
      isNormalUser = true;
      shell = pkgs.fish;
      extraGroups = [ "audio" "video" "input" "wheel" ];
      #password = "1";
      hashedPasswordFile = "/persist/home/secrets/passwd-flakes";
      packages = [ pkgs.home-manager ];
    };
  };

  home-manager = {
    users.flakes = import ../../home/flakes/home.nix;
    useUserPackages = true;
    extraSpecialArgs = { inherit inputs outputs; };
  };

  services = {
    hardware = {
      openrgb.enable = true;
      openrgb.motherboard = "intel";
    };
    blueman.enable = true;
  };

  hardware = {
    bluetooth.enable = true;
    bluetooth.settings = {
      General = {
        Enable = "Source,Sink,Media,Socket";
      };
    };
    i2c.enable = true;

    cpu.intel.updateMicrocode = true;
    opengl = {
      enable = true;
      # extraPackages = with pkgs; [ amdvlk ];
      driSupport = true;
      driSupport32Bit = true;
    };
    opentabletdriver.enable = true;
  };

  systemd.user.services.telephony_client.enable = false;

  system.stateVersion = "23.05";

}
