{
  description = "fk this shit?";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    impermanence.url = "github:nix-community/impermanence";
    hardware.url = "github:nixos/nixos-hardware";
    hyprpaper.url ="github:hyprwm/hyprpaper";
    nix-colors.url = "github:misterio77/nix-colors";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hyprland = {
      url = "github:hyprwm/hyprland";
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
    hyprwm-contrib.url = "github:hyprwm/contrib";
    hyprpicker.url = "github:hyprwm/hyprpicker";
    hyprland-portal.url = "github:hyprwm/xdg-desktop-portal-hyprland";
    firefox-addons.url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";
    agenix.url = "github:ryantm/agenix";
    aagl = {
      url = "github:ezKEa/aagl-gtk-on-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-index-db = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    matugen.url = "github:InioX/matugen";
    ags = {
      url = "github:Aylur/ags";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    eww = {
      url = "github:ralismark/eww/tray-3";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        rust-overlay.follows = "rust-overlay";
      };
    };
  };

  outputs = { self, nixpkgs, home-manager, agenix, hyprland, aagl, ... } @ inputs:
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
    # packages = forEachPkgs (pkgs: (import ./pkgs { inherit pkgs; }) // {
    #     neovim = let
    #       homeCfg = mkHome [ ./home/misterio/generic.nix ] pkgs;
    #     in pkgs.writeShellScriptBin "nvim" ''
    #       ${homeCfg.config.programs.neovim.finalPackage}/bin/nvim \
    #       -u ${homeCfg.config.xdg.configFile."nvim/init.lua".source} \
    #       "$@"
    #     '';
    #   });
    devShells = forEachPkgs (pkgs: import ./shell.nix { inherit pkgs; });
    formatter = forEachPkgs (pkgs: pkgs.nixpkgs-fmt);

    nixosConfigurations = {
      flakes = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs outputs; };
        modules = [ ./system/snow ];
      };
    };  

    nixosConfigurations = {
      drops = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs outputs; };
        modules = [ ./system/dew ];
      };
    };  

    homeConfigurations = {
      "Snow@flakes" = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages."x86_64-linux";
      extraSpecialArgs = { inherit inputs outputs; };
      modules = [ ./home/flakes/home.nix ];
      };
    };

    homeConfigurations = {
      "Dew@drops" = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages."x86_64-linux";
      extraSpecialArgs = { inherit inputs outputs; };
      modules = [ ./home/drops/home.nix ];
      };
    };

  };
}
