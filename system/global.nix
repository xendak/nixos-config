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
in
{
  imports = [
    inputs.impermanence.nixosModules.impermanence
    inputs.home-manager.nixosModules.home-manager
    inputs.agenix.nixosModules.default

    ./extras/postMountFiles.nix
    ./extras/tailscale.nix
    ./extras/bash.nix
    ./extras/fish.nix
    ./extras/fonts.nix

    ./extras/pipewire.nix
    ./extras/quietboot.nix

    ./hardware-configuration.nix
    ./openssh.nix
  ];

  nixpkgs = {
    # You can add overlays here
    overlays = [
      # Add overlays your own flake exports (from overlays and pkgs dir):
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages

      inputs.agenix.overlays.default
      # inputs.ragenix.overlays.default

      # Eww systray
      inputs.rust-overlay.overlays.default

      # You can also add overlays exported from other flakes:
      # neovim-nightly-overlay.overlays.default

      # Or define it inline, for example:
      # (final: prev: {
      #   hi = final.hello.overrideAttrs (oldAttrs: {
      #     patches = [ ./change-hello-to-hi.patch ];
      #   });
      # })
    ];
    # Configure your nixpkgs instance
    config = {
      # Disable if you don't want unfree packages
      allowUnfree = true;
      chromium.commandLineArgs = "--gtk-version=4 --enable-features=UseOzonePlatform --ozone-platform=wayland";
    };
  };

  #:Agenix :Keys
  age = {
    identityPaths = [ "/persist/etc/ssh/ssh_host_ed25519_key" ];

    secrets = {
      github-token = {
        file = ../secrets/github-token.age;
        owner = "root";
        group = "root";
      };

      nix-cache = {
        file = ../secrets/nix-cache.age;
        symlink = false;
        name = "cache-key.priv";
        owner = "root";
        group = "root";
        mode = "600";
      };

      pw = {
        file = ../secrets/pw.age;
        symlink = false;
        name = "id_ed25519";
        owner = "root";
        group = "nixbld";
        mode = "600";
      };

      nix-builder = {
        file = ../secrets/nix-builder.age;
        symlink = false;
        name = "nix_ed25519";
        owner = "root";
        group = "nixbld";
        mode = "0440";
      };

      gemini-api-key = {
        file = ../secrets/gemini-api-key.age;
        symlink = false;
        name = "gemini";
        owner = "xendak";
        group = "users";
        mode = "600";
      };

      steamgriddb = {
        file = ../secrets/steamgriddb.age;
        symlink = false;
        name = "steam";
        owner = "xendak";
        group = "users";
        mode = "600";
      };
    };
  };

  systemd.services = {
    "agenix-secrets" = {
      wantedBy = [ "default.target" ];
      wants = [ "agenix.service" ];
      after = [
        "agenix.service"
        "home-manager-xendak.service"
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
            '';
          in
          "${script}";
      };
    };
  };

  nix = {
    # extraOptions = ''access-tokens = github.com=${config.age.secrets.github-token.path}'';
    # extraOptions = ''access-tokens = github.com=${builtins.readFile config.age.secrets.github-token.path}'';
    # This will add each flake input as a registry
    # To make nix3 commands consistent with your flake
    registry = lib.mapAttrs (_: value: { flake = value; }) inputs;

    # This will additionally add your inputs to the system's legacy channels
    # Making legacy nix commands consistent as well, awesome!
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;

    settings = {
      trusted-users = [
        "root"
        "@wheel"
      ];
      # was getting a warning
      download-buffer-size = 524288000;
      experimental-features = "nix-command flakes";
      auto-optimise-store = true;
    };
  };

  # Adds non sudo wally cli
  security = {
    pam.services.swaylock = { };
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

  hardware.enableRedistributableFirmware = true;

  # root xdg
  environment.etc."xdg/Xresources".text = ''
    Xcursor.size: 32
    Xcursor.theme: Bibata-Modern-Classic
  '';

  qt = {
    enable = true;
    # do i even need this here?
    platformTheme = "qt5ct";
  };

  environment.variables = {
    EDITOR = "hx";
    SUDO_EDITOR = "vi";
  };

  programs.steam = {
    remotePlay.openFirewall = true;
    gamescopeSession.enable = true;
    dedicatedServer.openFirewall = true;
  };

  environment.systemPackages = [
    pkgs.ntfs3g
    pkgs.i2c-tools
    config.boot.kernelPackages.cpupower
    pkgs.networkmanagerapplet

    pkgs.ripgrep
    pkgs.fd
    pkgs.eza
    pkgs.bat

    pkgs.helix

    pkgs.busybox
    pkgs.lm_sensors
    pkgs.agenix

    pkgs.xdg-desktop-portal-wlr
    pkgs.xdg-desktop-portal-hyprland

    pkgs.xdg-desktop-portal-termfilechooser
    pkgs.papirus-icon-theme
    pkgs.papirus-folders
    pkgs.adwaita-icon-theme
    pkgs.qogir-icon-theme
    pkgs.morewaita-icon-theme
  ];

  xdg.portal = {
    enable = true;
    config = {
      common = {
        default = [ "gnome" ];
        "org.freedesktop.impl.portal.FileChooser" = [ "termfilechooser" ];
      };
      hyprland = {
        default = [
          "hyprland"
          "gtk"
        ];
        "org.freedesktop.impl.portal.FileChooser" = [ "termfilechooser" ];
        "org.freedesktop.impl.portal.Access" = "gtk";
        "org.freedesktop.impl.portal.Notification" = "gtk";
      };
      niri = {
        default = [
          "gnome"
          "gtk"
        ];
        "org.freedesktop.impl.portal.FileChooser" = [ "termfilechooser" ];
        "org.freedesktop.impl.portal.Access" = "gnome";
        "org.freedesktop.impl.portal.Notification" = "gnome";

      };

    };
    wlr.enable = true;
    extraPortals = [
      pkgs.xdg-desktop-portal-hyprland
      pkgs.xdg-desktop-portal-termfilechooser
      pkgs.xdg-desktop-portal-gtk
      pkgs.xdg-desktop-portal-gnome
      # pkgs.kdePackages.xdg-desktop-portal-kde
      pkgs.xdg-desktop-portal-wlr
    ];
    #configPackages = [ pkgs.inputs.hyprland.hyprland ];
  };

  # Gamemode
  programs.gamemode = {
    enable = true;
    enableRenice = true;
    settings = {
      general = {
        softrealtime = "auto";
        renice = 10;
      };
      custom = {
        start = "notify-send -a 'Gamemode' 'Optimizations activated'";
        end = "notify-send -a 'Gamemode' 'Optimizations deactivated'";
      };
    };
  };

  # Persistence -----------------------------
  programs.fuse.userAllowOther = true;
  programs.dconf.enable = true;
  # environment.enableAllTerminfo = true;
  environment.persistence."/persist" = {
    hideMounts = true;
    directories = [
      "/etc/nixos"
      "/etc/NetworkManager"
      "/var/lib/systemd"
      "/var/lib/bluetooth"
      "/var/lib/nixos"
      "/var/lib/NetworkManager"
    ];
    files = [
      "/etc/machine-id"
      "/etc/ssh/ssh_host_ed25519_key.pub"
      "/etc/ssh/ssh_host_ed25519_key"
      "/etc/ssh/ssa_host_rsa_key.pub"
      "/etc/ssh/ssh_host_ecdsa_key.pub"
    ];
  };

  # Services -----------------------------
  services.dbus.packages = [
    pkgs.gcr
  ];
  services = {

    blueman.enable = true;
    avahi = {
      enable = true;
      openFirewall = true;
      nssmdns4 = true;
    };
    udev.packages = with pkgs; [ gnome-settings-daemon ];

    udisks2.enable = true;
    fstrim.enable = true;
    speechd.enable = false;

    xserver.desktopManager.runXdgAutostartIfNone = true;
    xserver.xkb = {
      layout = "mine";
      extraLayouts = {
        mine = {
          description = "My custom xkb layout.";
          languages = [ "eng" ];
          symbolsFile = ./extras/custom_layout.xkb;
        };
      };
    };
  };

  # localtime specific
  time.timeZone = lib.mkDefault "America/Sao_Paulo";
  time.hardwareClockInLocalTime = false;
  # services.localtimed.enable = true;
  services.automatic-timezoned.enable = true;
  services.geoclue2.enable = true;
  services.geoclue2.geoProviderUrl = "https://api.beacondb.net/v1/geolocate";
  location.provider = "geoclue2";

  # locale configs
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

  # polkit agent
  security.polkit.enable = true;
  security.rtkit.enable = true;
  systemd = {
    user.services.polkit-gnome-authentication-agent-1 = {
      description = "polkit-gnome-authentication-agent-1";
      wantedBy = [ "graphical-session.target" ];
      wants = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
        Restart = "on-failure";
        RestartSec = 1;
        TimeoutStopSec = 10;
      };
    };
  };

  systemd.user.services.telephony_client.enable = false;

  system.stateVersion = lib.mkDefault "25.05";
}
