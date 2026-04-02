# macbook(2012) air a1
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix

    ../global.nix

    ../extras/btrfs-optin-persistence.nix
    ../extras/xendak.nix
    ../extras/remote-builder.nix
    ../extras/bluetooth.nix

    ../extras/sync-browser.nix
    ../extras/greetd.nix

    inputs.hardware.nixosModules.common-cpu-intel
    inputs.hardware.nixosModules.common-pc-ssd
    inputs.auto-cpufreq.nixosModules.default
  ];

  networking.hostName = "Rain";

  # old macbook broadcom i guess
  nixpkgs.config.allowInsecurePredicate =
    pkg:
    builtins.elem (lib.getName pkg) [
      "broadcom-sta"
    ];

  nixpkgs.config.permittedInsecurePackages = [
    "broadcom-sta-6.30.223.271-59-6.18.19"
    "broadcom-sta-6.30.223.271-59-6.18.13"
  ];

  boot = {

    kernelPackages = pkgs.linuxKernel.packages.linux_zen;
    extraModulePackages = [ config.boot.kernelPackages.broadcom_sta ];
    # https://forum.manjaro.org/t/kworker-kacpid-over-70-of-cpu-dual-boot-mac-manjaro/61981
    kernelParams = [
      "acpi_osi=Darwin"
      "atmel_mxt_ts.enable_multitouch=1"
      "hid_apple.swap_opt_cmd=1"
      "hid_apple.swap_fn_leftctrl=1"
      "hid_apple.iso_layout=0"
      "acpi_backlight=video"
      "acpi_mask_gpe=0x15"
    ];
    supportedFilesystems = [
      "btrfs"
    ];

    initrd.availableKernelModules = [
      "uhci_hcd"
      "ehci_pci"
      "ahci"
      "usbhid"
      "usb_storage"
      "sd_mod"
    ];
    kernelModules = [
      "kvm-intel"
      "i2c-dev"
      "i2c-i801"
      "coretemp"
      "wl"
    ];
    loader.systemd-boot.enable = true;
  };

  services.greetd = {
    settings = {
      initial_session = {
        command = "niri-session";
        user = "xendak";
      };
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
    pkgs.adwaita-icon-theme
    config.boot.kernelPackages.cpupower
  ];

  location.provider = "geoclue2";

  # User & Host -----------------------------
  nix = {
    buildMachines = [
      {
        hostName = "Snow";
        sshUser = "xendak";
        sshKey = "/etc/ssh/ssh_host_ed25519_key";
        system = "x86_64-linux";
        protocol = "ssh-ng";
        maxJobs = 20;
        speedFactor = 3;
        supportedFeatures = [
          "nixos-test"
          "benchmark"
          "big-parallel"
          "kvm"
        ];
      }
    ];
    distributedBuilds = true;
    settings = {
      builders-use-substitutes = true;
      fallback = true;
      warn-dirty = false;
      connect-timeout = 10;
      substituters = [
        # "ssh-ng://xendak@Snow"
        "https://cache.nixos.org/"
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        # "Snow-1:ePOd1J2YyhEQZjXK3t/yA5Nt3aWFo4Bdp3ibjtW6Lpo="
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
  };

  # TODO: maybe use the same as desktop
  networking.networkmanager.enable = true;
  networking.useDHCP = lib.mkDefault true;
  networking.nameservers = [
    "8.8.8.8"
    "8.8.4.4"
  ];

  # laptop power management
  powerManagement.enable = true;
  powerManagement.cpuFreqGovernor = "schedutil";

  services = {
    mbpfan.enable = true;
    thermald.enable = true;
    acpid.enable = true;
    upower.enable = true;

    tlp = {
      enable = true;
      settings = {
        CPU_SCALING_GOVERNOR_ON_AC = "powersave";
        CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
        USB_AUTOSUSPEND_ON_BAT = 1;

        CPU_ENERGY_PERF_POLICY_ON_BAT = "balance_power";
        CPU_ENERGY_PERF_POLICY_ON_AC = "balance_performance";

        CPU_MIN_PERF_ON_AC = 0;
        CPU_MAX_PERF_ON_AC = 80;
        CPU_MIN_PERF_ON_BAT = 0;
        CPU_MAX_PERF_ON_BAT = 60;

        STOP_CHARGE_THRESH_BAT0 = 1;
        START_CHARGE_THRESH_BAT1 = 45;
        STOP_CHARGE_THRESH_BAT1 = 80;
      };
    };

  };

  console.font = "Lat2-Terminus16";

  hardware = {
    i2c.enable = true;

    cpu.intel.updateMicrocode = true;
    graphics = {
      extraPackages = with pkgs; [
        libvdpau-va-gl
        intel-media-driver
        intel-vaapi-driver
        libva-vdpau-driver
      ];
      enable = true;
      enable32Bit = true;
    };
    opentabletdriver.enable = true;
  };

  system.stateVersion = lib.mkDefault "25.05";
}
