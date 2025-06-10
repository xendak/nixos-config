# my main desktop
{
  config,
  lib,
  pkgs,
  inputs,
  outputs,
  ...
}:
{
  imports = [
    ../global.nix
    ./btrfs-optin-persistence.nix
    ./hardware-configuration.nix
    # ../extras/powersave.nix
    ../extras/kanata.nix

    ../extras/sync-browser.nix

    inputs.hardware.nixosModules.common-cpu-intel
    inputs.hardware.nixosModules.common-pc-ssd
    inputs.auto-cpufreq.nixosModules.default
  ];

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

  age.secrets.pw = {
    file = ../../secrets/pw.age;
    symlink = false;
    name = "id_ed25519";
    owner = "drops";
    group = "users";
    mode = "600";
  };
  age.identityPaths = [ "/persist/etc/ssh/ssh_host_ed25519_key" ];
  # environment.etc."something".source = "${config.age.secrets.pw.path}";

  systemd.services = {
    "agenix-secrets" = {
      wantedBy = [ "default.target" ];
      wants = [ "agenix.service" ];
      after = [
        "agenix.service"
        "home-manager-drops.service"
        "kanata-laptop.service"
      ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart =
          let
            script = pkgs.writeScript "myuser-start" ''
              #!${pkgs.runtimeShell}
              mkdir -p /home/drops/.ssh
              cat ${config.age.secrets.pw.path} > "/home/drops/.ssh/id_ed25519"
              chown drops:users /home/drops/.ssh/id_ed25519
              chmod 600 /home/drops/.ssh/id_ed25519
            '';
          in
          "${script}";
      };
    };
  };

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
    pkgs.adwaita-icon-theme
    config.boot.kernelPackages.cpupower
  ];

  location.provider = "geoclue2";

  # User & Host -----------------------------
  users = {
    mutableUsers = false;
    users.root = {
      hashedPasswordFile = "/persist/home/secrets/passwd-root";
    };
    users.drops = {
      isNormalUser = true;
      shell = pkgs.fish;
      extraGroups = [
        "audio"
        "video"
        "input"
        "wheel"
        "networkmanager"
      ];
      hashedPasswordFile = "/persist/home/secrets/passwd-drops";
      packages = [ pkgs.home-manager ];
    };
  };

  networking.networkmanager.enable = true;
  networking.useDHCP = lib.mkDefault true;
  networking.nameservers = [
    "8.8.8.8"
    "8.8.4.4"
  ];
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
    useGlobalPkgs = true;
    extraSpecialArgs = { inherit inputs outputs; };
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

    blueman.enable = true;
  };

  environment.etc."/bluetooth/main.conf".text = ''
    [General]
    ControllerMode=dual
    Enable=Source,Sink,Media,Socket

    [Policy]
    AutoEnable=false
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

  systemd.user.services.telephony_client.enable = false;

  system.stateVersion = "25.05";
}
