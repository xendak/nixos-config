{
  pkgs,
  config,
  inputs,
  outputs,
  lib,
  ...
}:
{
  imports = [
    inputs.home-manager.nixosModules.home-manager
    # inputs.agenix.nixosModules.default
    ../extras/fish.nix
  ];
  time.timeZone = "America/Sao_Paulo";


  # programs.fish.enable = true;
  programs.dconf.enable = true;
  environment.pathsToLink = ["/share/fish"];
  environment.shells = [pkgs.fish];

  environment.enableAllTerminfo = true;

  #security.sudo.wheelNeedsPassword = false;

  networking.useDHCP = lib.mkDefault true;
  # networking.nameservers = ["8.8.8.8" "8.8.4.4"];
  networking.hostName = "wsl";

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

  # FIXME: uncomment the next line to enable SSH
  # services.openssh.enable = true;

  users.users.nixos = {
    isNormalUser = true;
    # FIXME: change your shell here if you don't want fish
    shell = pkgs.fish;
    extraGroups = [ "wheel" "docker" "input" "audio" "video" ];
    hashedPassword= "$6$/XlEv26WR0fDTVnf$43kBq/CR1oQ4x5R70xmWUuPlaf1aoHr7G5c6FajQv6ibJ5aFKafokHrSMcDp3itve5JSroM92O29KICplH4vz.";
    #hashedPasswordFile = "/mnt/g/persist/home/secrets/passwd-flakes";
    packages = [ pkgs.home-manager ];
  };
  users.users.root = {
    #hashedPasswordFile = "/mnt/g/persist/home/secrets/passwd-root"
    hashedPassword = "$6$4HwRzWTtgI6oR.Fp$BJM7NNDt.kFKBjjiZxOEhO2rVU9v7iRiYVXejjXuq14RIQ4INP6m3JGp7G7TlesAWUkcEiXO0UEDGnjPXwxGQ1";
  };

  home-manager = {
    backupFileExtension = "hm-backup";
    users.nixos = import ../../home/nixos/home.nix;
    useUserPackages = true;
    extraSpecialArgs = {inherit inputs outputs;};
  };

  system.stateVersion = "24.05";

  wsl = {
    enable = true;
    wslConf.automount.root = "/mnt";
    wslConf.interop.appendWindowsPath = false;
    wslConf.network.generateHosts = false;
    defaultUser = "nixos";
    startMenuLaunchers = true;

    # Enable integration with Docker Desktop (needs to be installed)
    docker-desktop.enable = false;
  };

  # virtualisation.docker = {
  #   enable = true;
  #   enableOnBoot = true;
  #   autoPrune.enable = true;
  # };


  nixpkgs = {
    # overlays = [
    #   outputs.overlays.additions
    #   outputs.overlays.modifications
    #   outputs.overlays.unstable-packages
    #   # inputs.agenix.overlays.default
    #   inputs.rust-overlay.overlays.default
    # ];
    config = {
      allowUnfree = true;
    };
  };

  nix = {
    settings = {
      trusted-users = ["nixos"];
      accept-flake-config = true;
      auto-optimise-store = true;
    };

    registry = {
      nixpkgs = {
        flake = inputs.nixpkgs;
      };
    };

    nixPath = [
      "nixpkgs=${inputs.nixpkgs.outPath}"
      "nixos-config=/etc/nixos/configuration.nix"
      "/nix/var/nix/profiles/per-user/root/channels"
    ];

    package = pkgs.nixVersions.stable;
    extraOptions = ''experimental-features = nix-command flakes'';

    gc = {
      automatic = true;
      options = "--delete-older-than 7d";
    };
  };

  security.sudo = {
    enable = true;
    extraRules = [
      {
        commands = [
          {
            command = "${lib.getExe pkgs.wally-cli}";
            options = ["NOPASSWD"];
          }
        ];
        groups = ["wheel"];
      }
    ];
  };
}