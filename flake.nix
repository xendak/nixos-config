{
  description = "eep";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-25.11";
    # impermanence.url = "github:nix-community/impermanence";
    # impermanence.url = "github:misterio77/impermanence";
    impermanence = {
      # https://github.com/nix-community/impermanence/pull/272#discussion_r2230796215
      url = "github:misterio77/impermanence";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };
    hardware = {
      url = "github:nixos/nixos-hardware";
      # inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # :mine
    zellij-pane-toggle = {
      url = "path:./pkgs/zellij-pane-toggle";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    gobuild = {
      url = "github:xendak/gobuild";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # others

    hyprland = {
      url = "git+https://github.com/hyprwm/Hyprland?submodules=1";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hyprland-plugins = {
      url = "github:hyprwm/hyprland-plugins";
      inputs.hyprland.follows = "hyprland";
    };
    xdph = {
      url = "github:hyprwm/xdg-desktop-portal-hyprland";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hyprwm-contrib = {
      url = "github:hyprwm/contrib";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hyprpicker = {
      url = "github:hyprwm/hyprpicker";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hyprland-portal = {
      url = "github:hyprwm/xdg-desktop-portal-hyprland";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    firefox-addons = {
      url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    auto-cpufreq = {
      url = "github:AdnanHodzic/auto-cpufreq";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-index-db = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # gesture app launcher
    hexecute = {
      url = "github:ThatOtherAndrew/Hexecute";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    wezterm.url = "github:wez/wezterm?dir=nix";
    wezterm-floating.url = "github:yasunogithub/wezterm?ref=floating-pane&dir=nix";
    # helix-flake.url = "github:helix-editor/helix";
    # matugen.url = "github:InioX/matugen";

    niri = {
      url = "github:sodiboo/niri-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # kinda liked it a lot :^)
    zig = {
      url = "github:mitchellh/zig-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    zls = {
      url = "github:zigtools/zls";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    uwu-colors = {
      url = "github:q60/uwu_colors";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ags = {
      url = "github:Aylur/ags/v1";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    quickshell = {
      url = "git+https://git.outfoxxed.me/outfoxxed/quickshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # terminal filepicker
    xdg-termfilepickers = {
      url = "github:Guekka/xdg-desktop-portal-termfilepickers";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # #github:owner/repo?rev=
    # NIXOS WSL
    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    zen-browser = {
      url = "github:0xc000022070/zen-browser-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-stable,
      home-manager,
      xdg-termfilepickers,
      agenix,
      hyprland,
      nixos-wsl,
      ...
    }@inputs:
    let
      inherit (self) outputs;
      forEachSystem = nixpkgs.lib.genAttrs [ "x86_64-linux" ];
      forEachPkgs = f: forEachSystem (sys: f nixpkgs.legacyPackages.${sys});
    in
    {
      templates = import ./templates;
      overlays = import ./overlays { inherit inputs; };
      homeManagerModules = import ./modules/home-manager;

      packages = forEachPkgs (pkgs: import ./pkgs { inherit pkgs; });
      devShells = forEachPkgs (pkgs: import ./shell.nix { inherit pkgs; });
      formatter = forEachPkgs (pkgs: pkgs.nixpkgs-fmt);

      nixosConfigurations = {
        Snow = nixpkgs.lib.nixosSystem {
          specialArgs = {
            inherit inputs outputs;
          };
          modules = [ ./system/Snow ];
        };
        Dew = nixpkgs.lib.nixosSystem {
          specialArgs = {
            inherit inputs outputs;
          };
          modules = [ ./system/Dew ];
        };
        Rain = nixpkgs.lib.nixosSystem {
          specialArgs = {
            inherit inputs outputs;
          };
          modules = [ ./system/Rain ];
        };
        wsl = nixpkgs.lib.nixosSystem {
          specialArgs = {
            inherit inputs outputs;
          };
          modules = [
            nixos-wsl.nixosModules.default
            ./system/wsl
          ];
        };

        iso = nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs outputs; };
          modules = [
            "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
            ./system/global.nix
            (
              {
                pkgs,
                config,
                lib,
                ...
              }:
              {
                system.stateVersion = lib.mkForce "25.05";

                networking.networkmanager.enable = true;
                hardware.enableRedistributableFirmware = true;
                services.openssh.settings.PermitRootLogin = lib.mkForce "yes";

                nixpkgs.config.allowUnfree = true;
                boot.kernelModules = [ "wl" ];
                boot.extraModulePackages = [ config.boot.kernelPackages.broadcom_sta ];
                nixpkgs.config.permittedInsecurePackages = [
                  "broadcom-sta-6.30.223.271-59-6.18.19"
                ];

                users.users.root.openssh.authorizedKeys.keys = [
                  (builtins.readFile ./home/common/ssh/id_ed25519.pub)
                  (builtins.readFile ./system/Snow/ssh_host_ed25519_key.pub)
                  (builtins.readFile ./system/Dew/ssh_host_ed25519_key.pub)
                ];

                users.users.xendak = {
                  isNormalUser = true;
                  group = "users";
                };

                # users.users.xendak.extraGroups = [ "wheel" ];

                programs.ssh.knownHostsFiles = [
                  ./home/common/ssh/known_hosts
                ];
                environment.systemPackages = [
                  pkgs.git
                  pkgs.helix
                  pkgs.openssh
                  pkgs.neovim
                ];
              }
            )
          ];
        };
      };

      homeConfigurations = {
        "xendak@Snow" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages."x86_64-linux";
          extraSpecialArgs = {
            inherit inputs outputs;
            host = "Snow";
          };
          modules = [ ./home/xendak.nix ];
        };
        "xendak@Dew" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages."x86_64-linux";
          extraSpecialArgs = {
            inherit inputs outputs;
            host = "Dew";
          };
          modules = [ ./home/xendak.nix ];
        };
        "xendak@Rain" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages."x86_64-linux";
          extraSpecialArgs = {
            inherit inputs outputs;
            host = "Rain";
          };
          modules = [ ./home/xendak.nix ];
        };
        "nixos@wsl" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages."x86_64-linux";
          extraSpecialArgs = {
            inherit inputs outputs;
            host = "wsl";
          };
          modules = [ ./home/xendak.nix ];
        };
      };
    };
}
