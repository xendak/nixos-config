{
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
    ../extras/kanata.nix

    inputs.hardware.nixosModules.common-cpu-intel
    inputs.hardware.nixosModules.common-pc-ssd
    inputs.auto-cpufreq.nixosModules.default
  ];

  networking.hostName = "Dew";

  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "ahci"
    "nvme"
    "usbhid"
    "usb_storage"
    "sd_mod"
  ];
  boot.kernelModules = [
    "kvm-intel"
    "i2c-dev"
    "i2c-i801"
    "coretemp"
  ];
  boot.loader.systemd-boot.enable = true;
  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_zen;
    # kernelPackages = pkgs.linuxPackages_xanmod_latest;
    kernelParams = [
      "amd_pstate=active"
      "ideapad_laptop.allow_v4_dytc=Y"
      ''acpi_osi="Windows 2020"''
    ];
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

  # NTFS-3G for Windows Partititions
  environment.systemPackages = [
    pkgs.brightnessctl
    pkgs.acpi
  ];

  networking.networkmanager.enable = true;
  networking.useDHCP = lib.mkDefault true;
  networking.nameservers = [
    "8.8.8.8"
    "8.8.4.4"
  ];

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

  # laptop power management
  services = {
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

  hardware = {
    i2c.enable = true;

    cpu.intel.updateMicrocode = true;
    graphics = {
      enable = true;
      enable32Bit = true;
    };
    opentabletdriver.enable = true;
  };

}
