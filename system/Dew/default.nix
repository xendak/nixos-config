# my main desktop
{ config, lib, pkgs, inputs, outputs, ...}: {
  imports = [
    ../global.nix
    ./btrfs-optin-persistence.nix
    ./hardware-configuration.nix
    ../extras/powersave.nix

    inputs.hardware.nixosModules.common-cpu-intel
    inputs.hardware.nixosModules.common-gpu-amd
    inputs.hardware.nixosModules.common-pc-ssd
    inputs.auto-cpufreq.nixosModules.default 

  ];


  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod" ];
  boot.kernelModules = [ "kvm-intel" "i2c-dev" "i2c-i801" "coretemp" ];
  boot.loader.systemd-boot.enable = true;
  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_zen;
    # kernelPackages = pkgs.linuxPackages_xanmod_latest;
    kernelParams = [
      "amd_pstate=active"
      "ideapad_laptop.allow_v4_dytc=Y"
      ''acpi_osi="Windows 2020"''
    ];
    supportedFilesystems = [ "btrfs" "ntfs" ];
  };

  # genshin
  #programs.anime-game-launcher.enable = true;
  #programs.honkers-railway-launcher.enable = true;
  programs.auto-cpufreq.enable = true;
    # optionally, you can configure your auto-cpufreq settings, if you have any
    programs.auto-cpufreq.settings = {
    charger = {
      governor = "performance";
      turbo = "auto";
    };

    battery = {
      governor = "powersave";
      turbo = "auto";
    };
  };
  # NTFS-3G for Windows Partititions
  environment.systemPackages = [
    pkgs.ntfs3g
    pkgs.i2c-tools
    pkgs.networkmanager_dmenu
    pkgs.networkmanagerapplet
    pkgs.brightnessctl
    pkgs.acpi
    pkgs.qogir-icon-theme
    pkgs.morewaita-icon-theme
    pkgs.gnome.adwaita-icon-theme
    config.boot.kernelPackages.cpupower
  ];


  # User & Host -----------------------------
  users = {
    mutableUsers = false;
    users.root = {
      hashedPasswordFile = "/persist/home/secrets/passwd-root";
    };
    users.drops = {
      isNormalUser = true;
      shell = pkgs.fish;
      extraGroups = [ "audio" "video" "input" "wheel" ];
      #password = "1";
      hashedPasswordFile = "/persist/home/secrets/passwd-drops";
      packages = [ pkgs.home-manager ];
    };
  };

  networking.networkmanager.enable = true;
  networking.useDHCP = lib.mkDefault true;
  networking.nameservers = [ "8.8.8.8" "8.8.4.4" ];
  networking.hostName = "Dew";
  environment.persistence."/persist" = {
    hideMounts = true;
    directories = [
      "/etc/NetworkManager"
      "/var/lib/NetworkManager"
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

  home-manager = {
    users.drops = import ../../home/drops/home.nix;
    useUserPackages = true;
    extraSpecialArgs = { inherit inputs outputs; };
  };

  # laptop power management
  services = {
    acpid.enable = true;
    upower.enable = true;
    fstrim.enable = true;

    # tlp = {
    #   enable = true;
    #   settings = {
    #     CPU_SCALING_GOVERNOR_ON_AC = "performance";
    #     CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

    #     CPU_ENERGY_PERF_POLICY_ON_BAT = "power";
    #     CPU_ENERGY_PERF_POLICY_ON_AC = "performance";

    #     CPU_MIN_PERF_ON_AC = 0;
    #     CPU_MAX_PERF_ON_AC = 100;
    #     CPU_MIN_PERF_ON_BAT = 0;
    #     CPU_MAX_PERF_ON_BAT = 70;

    #     #Optional helps save long term battery health
    #     START_CHARGE_THRESH_BAT1 = 20; # 40 and bellow it starts to charge
    #     STOP_CHARGE_THRESH_BAT1 = 85; # 80 and above it stops charging
    #     STOP_CHARGE_THRESH_BAT0 = 1; # 80 and above it stops charging
    #   };
    # };
  

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
