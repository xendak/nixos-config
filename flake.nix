{
  description = "fk this shit?";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.05";
    # impermanence.url = "github:nix-community/impermanence";
    impermanence.url = "github:misterio77/impermanence";
    hardware.url = "github:nixos/nixos-hardware";
    nix-colors.url = "github:misterio77/nix-colors";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hyprland = {
      # url = "github:hyprwm/hyprland";
      url = "git+https://github.com/hyprwm/Hyprland?submodules=1";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    suyu.url = "github:Noodlez1232/suyu-flake";
    hyprland-plugins = {
      url = "github:hyprwm/hyprland-plugins";
      inputs.hyprland.follows = "hyprland";
    };
    xdph = {
      url = "github:hyprwm/xdg-desktop-portal-hyprland";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    swayfx.url = "github:WillPower3309/swayfx";
    hyprwm-contrib.url = "github:hyprwm/contrib";
    hyprpicker.url = "github:hyprwm/hyprpicker";
    hyprland-portal.url = "github:hyprwm/xdg-desktop-portal-hyprland";
    firefox-addons.url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";
    agenix.url = "github:ryantm/agenix";
    aagl = {
      url = "github:ezKEa/aagl-gtk-on-nix";
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

    helix = {
      url = "github:pascalkuthe/helix?rev=e86b77843b8756d53cc756837f91aa8d368495bc";
    };

    matugen.url = "github:InioX/matugen?rev=e795cda353235e503bb292f56bdd6573974f4add";
    # github:owner/repo?rev=

    ags = {
      url = "github:Aylur/ags";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    # NIXOS WSL
    nixos-wsl.url = "github:nix-community/NixOS-WSL/main";
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    agenix,
    hyprland,
    aagl,
    helix,
    nixos-wsl,
    ...
  } @ inputs: let
    inherit (self) outputs;
    forEachSystem = nixpkgs.lib.genAttrs ["x86_64-linux"];
    forEachPkgs = f: forEachSystem (sys: f nixpkgs.legacyPackages.${sys});
  in {
    templates = import ./templates;
    overlays = import ./overlays {inherit inputs;};
    homeManagerModules = import ./modules/home-manager;

    packages = forEachPkgs (pkgs: import ./pkgs {inherit pkgs;});
    devShells = forEachPkgs (pkgs: import ./shell.nix {inherit pkgs;});
    formatter = forEachPkgs (pkgs: pkgs.nixpkgs-fmt);

    nixosConfigurations = {
      flakes = nixpkgs.lib.nixosSystem {
        specialArgs = {
          inherit inputs outputs;
        };
        modules = [./system/Snow];
      };
    };
    nixosConfigurations = {
      drops = nixpkgs.lib.nixosSystem {
        specialArgs = {
          inherit inputs outputs;
        };
        modules = [./system/Dew];
      };
    };
    
    nixosConfigurations = {
      nixos = nixpkgs.lib.nixosSystem {
        specialArgs = {
          inherit inputs outputs;
        };
        modules = [
	  nixos-wsl.nixosModules.default
	  ./system/wsl
	];
      };
    };

    homeConfigurations = {
      "Snow@flakes" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages."x86_64-linux";
        extraSpecialArgs = {
          inherit inputs outputs;
        };
        modules = [./home/flakes/home.nix];
      };
    };

    homeConfigurations = {
      "Dew@drops" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages."x86_64-linux";
        extraSpecialArgs = {
          inherit inputs outputs;
        };
        modules = [./home/drops/home.nix];
      };
    };

    homeConfigurations = {
      "wsl@nixos" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages."x86_64-linux";
        extraSpecialArgs = {
          inherit inputs outputs;
        };
        modules = [./home/nixos/home.nix];
      };
    };
  };
}
