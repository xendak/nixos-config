# macbook(2012) air a1
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

    ../extras/sync-browser.nix
    ../extras/greetd.nix

    inputs.hardware.nixosModules.common-cpu-intel
    inputs.hardware.nixosModules.common-pc-ssd
    inputs.auto-cpufreq.nixosModules.default
  ];

  # old macbook broadcom i guess
  nixpkgs.config.permittedInsecurePackages = [
    "broadcom-sta-6.30.223.271-59-6.18.19"
  ];

  boot = {

    kernelPackages = pkgs.linuxKernel.packages.linux_zen;
    extraModulePackages = [ config.boot.kernelPackages.broadcom_sta ];
    # https://forum.manjaro.org/t/kworker-kacpid-over-70-of-cpu-dual-boot-mac-manjaro/61981
    kernelParams = [
      "acpi_osi=Darwin"
      "atmel_mxt_ts.enable_multitouch=1"
      "hid_apple.swap_opt_cmd=1"
      "hid_apple.iso_layout=0"
      "acpi_backlight=vendor"
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

  age.secrets.gemini-api-key = {
    file = ../../secrets/gemini-api-key.age;
    symlink = false;
    name = "gemini";
    owner = "xendak";
    group = "users";
    mode = "600";
  };
  age.secrets.steamgriddb = {
    file = ../../secrets/steamgriddb.age;
    symlink = false;
    name = "steam";
    owner = "xendak";
    group = "users";
    mode = "600";
  };
  age.secrets.pw = {
    file = ../../secrets/pw.age;
    symlink = false;
    name = "id_ed25519";
    owner = "xendak";
    group = "users";
    mode = "600";
  };

  systemd.services = {
    "agenix-secrets" = {
      wantedBy = [ "default.target" ];
      wants = [ "agenix.service" ];
      after = [
        "agenix.service"
        "home-manager-xendak.service"
        "kanata-laptop.service"
      ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart =
          let
            script = pkgs.writeScript "myuser-start" ''
              #!${pkgs.runtimeShell}
              mkdir -p /home/xendak/.ssh
              cat ${config.age.secrets.pw.path} > "/home/xendak/.ssh/id_ed25519"
              chown xendak:users /home/xendak/.ssh/id_ed25519
              chmod 600 /home/xendak/.ssh/id_ed25519
              cat ${config.age.secrets.gemini-api-key.path} > "/home/xendak/.ssh/gemini"
              chown xendak:users /home/xendak/.ssh/gemini
              chmod 600 /home/xendak/.ssh/gemini
              cat ${config.age.secrets.steamgriddb.path} > "/home/xendak/.ssh/steam"
              chown xendak:users /home/xendak/.ssh/steam
              chmod 600 /home/xendak/.ssh/steam
              rm -f /run/agenix/gemini
              rm -f /run/agenix/id_ed25519
              rm -f /run/agenix.d/1/gemini
              rm -f /run/agenix.d/1/id_ed25519
            '';
          in
          "${script}";
      };
    };
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
        # sshKey = "/persist/etc/ssh/ssh_host_ed25519_key";
        sshKey = config.age.secrets.pw.path;
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
        "ssh-ng://xendak@Snow"
        "https://cache.nixos.org/"
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "Snow-1:ePOd1J2YyhEQZjXK3t/yA5Nt3aWFo4Bdp3ibjtW6Lpo="
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
  };

  users = {
    mutableUsers = false;
    users.root = {
      hashedPasswordFile = "/persist/home/secrets/passwd-root";
    };
    users.xendak = {
      uid = 1000;
      isNormalUser = true;
      shell = pkgs.nushell;
      extraGroups = [
        "audio"
        "video"
        "input"
        "wheel"
        "networkmanager"
      ];
      hashedPasswordFile = "/persist/home/secrets/passwd-xendak";
      packages = [ pkgs.home-manager ];
    };
  };

  networking.networkmanager.enable = true;
  networking.useDHCP = lib.mkDefault true;
  networking.nameservers = [
    "8.8.8.8"
    "8.8.4.4"
  ];
  networking.hostName = "Rain";
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
    users.xendak = import ../../home/xendak.nix;
    useUserPackages = true;
    useGlobalPkgs = true;
    extraSpecialArgs = {
      inherit inputs outputs;
      host = "Rain";
    };
    backupFileExtension = "hm-backup";
    overwriteBackup = true;
  };

  # laptop power management
  powerManagement.enable = true;
  powerManagement.cpuFreqGovernor = "schedutil";
  hardware.opengl.extraPackages = with pkgs; [
    vaapiIntel
    vaapiVdpau
    libvdpau-va-gl
    intel-media-driver
  ];

  services = {
    mbpfan.enable = true;
    thermald.enable = true;
    acpid.enable = true;
    upower.enable = true;
    avahi = {
      enable = true;
      openFirewall = true;
      nssmdns4 = true;
    };

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

  console.font = "Lat2-Terminus16";
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

  system.stateVersion = lib.mkDefault "25.05";
}
