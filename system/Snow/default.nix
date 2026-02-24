{
  lib,
  config,
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

    ../extras/sync-browser.nix
    ../extras/greetd.nix

    inputs.hardware.nixosModules.common-cpu-intel
    inputs.hardware.nixosModules.common-gpu-amd
    inputs.hardware.nixosModules.common-pc-ssd

    inputs.aagl.nixosModules.default
  ];

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
  # boot.extraModulePackages = [pkgs.linuxKernel.packages.linux_zen.v4l2loopback];
  boot.extraModulePackages = with config.boot.kernelPackages; [ v4l2loopback ];
  # sudo modprobe v4l2loopback video_nr=2 card_label="VirtualCamera" exclusive_caps=1
  # modprobe v4l2loopback exclusive_caps=1 card_label='OBS Virtual Camera'
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

  age.secrets.pw = {
    file = ../../secrets/pw.age;
    symlink = false;
    name = "id_ed25519";
    owner = "flakes";
    group = "users";
    mode = "600";
  };
  age.secrets.gemini-api-key = {
    file = ../../secrets/gemini-api-key.age;
    symlink = false;
    name = "gemini";
    owner = "flakes";
    group = "users";
    mode = "600";
  };
  age.secrets.steamgriddb = {
    file = ../../secrets/steamgriddb.age;
    symlink = false;
    name = "steam";
    owner = "flakes";
    group = "users";
    mode = "600";
  };

  age.identityPaths = [ "/persist/etc/ssh/ssh_host_ed25519_key" ];
  # environment.etc."something".source = "${config.age.secrets.pw.path}";

  services.greetd = {
    settings = {
      initial_session = {
        command = "niri-session";
        user = "flakes";
      };
    };
  };

  systemd.services = {
    "agenix-secrets" = {
      wantedBy = [ "default.target" ];
      wants = [ "agenix.service" ];
      after = [
        "agenix.service"
        "home-manager-flakes.service"
      ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart =
          let
            script = pkgs.writeScript "myuser-start" ''
              #!${pkgs.runtimeShell}
              mkdir -p /home/flakes/.ssh
              cat ${config.age.secrets.pw.path} > "/home/flakes/.ssh/id_ed25519"
              chown flakes:users /home/flakes/.ssh/id_ed25519
              chmod 600 /home/flakes/.ssh/id_ed25519
              cat ${config.age.secrets.gemini-api-key.path} > "/home/flakes/.ssh/gemini"
              chown flakes:users /home/flakes/.ssh/gemini
              chmod 600 /home/flakes/.ssh/gemini
              cat ${config.age.secrets.steamgriddb.path} > "/home/flakes/.ssh/steam"
              chown flakes:users /home/flakes/.ssh/steam
              chmod 600 /home/flakes/.ssh/steam
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

  programs.steam = {
    remotePlay.openFirewall = true;
    gamescopeSession.enable = true;
    dedicatedServer.openFirewall = true;
  };

  environment.systemPackages = [
    pkgs.ntfs3g
    pkgs.openrgb-with-all-plugins
    pkgs.i2c-tools
    pkgs.qogir-icon-theme
    pkgs.morewaita-icon-theme
    pkgs.adwaita-icon-theme
    config.boot.kernelPackages.cpupower
    pkgs.linuxKernel.packages.linux_zen.v4l2loopback # uncertain if still needed here..?
    pkgs.v4l-utils

    # lact
    pkgs.lact

    # trying
    pkgs.networkmanager_dmenu
    pkgs.networkmanagerapplet
  ];

  # User & Host -----------------------------
  users = {
    mutableUsers = false;
    users.root = {
      hashedPasswordFile = "/persist/home/secrets/passwd-root";
    };
    users.flakes = {
      isNormalUser = true;
      shell = pkgs.nushell;
      extraGroups = [
        "audio"
        "video"
        "input"
        "wheel"
        "networkmanager"
      ];
      hashedPasswordFile = "/persist/home/secrets/passwd-flakes";
      openssh.authorizedKeys.keys = lib.splitString "\n" (
        builtins.readFile ../../home/common/ssh/id_ed25519.pub
      );
      packages = [ pkgs.home-manager ];
    };
  };

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
      "/etc/NetworkManager"
      "/var/lib/NetworkManager"
    ];
  };

  networking.hostName = "Snow";

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
    users.flakes = import ../../home/flakes/home.nix;
    useUserPackages = true;
    useGlobalPkgs = true;
    extraSpecialArgs = { inherit inputs outputs; };
    backupFileExtension = "hm-backup";
    overwriteBackup = true;
  };

  services = {
    hardware = {
      openrgb.enable = true;
      openrgb.motherboard = "intel";
    };

    blueman.enable = true;
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
      # extraPackages = with pkgs; [ amdvlk ];
      # driSupport = true;
      enable32Bit = true;
    };
    opentabletdriver.enable = true;
  };

  systemd.user.services.telephony_client.enable = false;

  system.stateVersion = "24.05";
}
