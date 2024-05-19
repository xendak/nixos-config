# my main desktop
{
  config,
  lib,
  pkgs,
  inputs,
  outputs,
  ...
}: {
  imports = [
    ../global.nix
    ./btrfs-optin-persistence.nix
    ./hardware-configuration.nix
    # ../extras/kanata.nix

    inputs.hardware.nixosModules.common-cpu-intel
    inputs.hardware.nixosModules.common-gpu-amd
    inputs.hardware.nixosModules.common-pc-ssd

    inputs.aagl.nixosModules.default
  ];

  boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod"];
  boot.kernelModules = ["kvm-intel" "amdgpu" "i2c-dev" "i2c-i801" "coretemp"];
  boot.loader.systemd-boot.enable = true;
  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_zen;
    # kernelPackages = pkgs.linuxPackages_xanmod_latest;
    supportedFilesystems = ["btrfs" "ntfs"];
  };

  age.secrets.pw = {
    file = ../../secrets/pw.age;
    symlink = false;
    name = "id_ed25519";
    owner = "flakes";
    group = "users";
    mode = "600";
  };
  age.identityPaths = ["/persist/etc/ssh/ssh_host_ed25519_key"];
  # environment.etc."something".source = "${config.age.secrets.pw.path}";

  systemd.services = {
    "agenix-secrets" = {
      wantedBy = ["default.target"];
      wants = ["agenix.service"];
      after = ["agenix.service" "home-manager-flakes.service"];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = let
          script = pkgs.writeScript "myuser-start" ''
            #!${pkgs.runtimeShell}
            mkdir -p /home/flakes/.ssh
            cat ${config.age.secrets.pw.path} > "/home/flakes/.ssh/id_ed25519"
            chown flakes:users /home/flakes/.ssh/id_ed25519
            chmod 600 /home/flakes/.ssh/id_ed25519
          '';
        in "${script}";
      };
    };
  };

  # genshin
  programs.anime-game-launcher.enable = true;
  programs.honkers-railway-launcher.enable = true;

  environment.systemPackages = let
    sddm-themes = pkgs.callPackage ../../../modules/sddm.nix {};
  in
  [
    sddm-themes.astronaut
    pkgs.ntfs3g
    pkgs.openrgb-with-all-plugins
    pkgs.i2c-tools
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
    users.flakes = {
      isNormalUser = true;
      shell = pkgs.fish;
      extraGroups = ["audio" "video" "input" "wheel"];
      #password = "1";
      hashedPasswordFile = "/persist/home/secrets/passwd-flakes";
      packages = [pkgs.home-manager];
    };
  };

  networking.useDHCP = lib.mkDefault true;
  networking.nameservers = ["8.8.8.8" "8.8.4.4"];
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
    extraSpecialArgs = {inherit inputs outputs;};
  };

  services = {
    displayManager.sddm = {
      enable = true;
      wayland.enable = true;
      theme = "astronaut";
      settings.Theme.CursorTheme = config.gtk.cursorTheme.name;
    };

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
    opengl = {
      enable = true;
      # extraPackages = with pkgs; [ amdvlk ];
      driSupport = true;
      driSupport32Bit = true;
    };
    opentabletdriver.enable = true;
  };

  systemd.user.services.telephony_client.enable = false;

  system.stateVersion = "23.11";
}
