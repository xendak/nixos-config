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
  };

  outputs = { self, nixpkgs, home-manager, agenix, hyprland, aagl, ... } @ inputs:
  let
    inherit (self) outputs;
    forEachSystem = nixpkgs.lib.genAttrs [ "x86_64-linux" ];
    forEachPkgs = f: forEachSystem (sys: f nixpkgs.legacyPackages.${sys});
  in
  {
    #nixosModules = import ./modules/nixos;
    overlays = import ./overlays { inherit inputs; };
    homeManagerModules = import ./modules/home-manager;

    packages = forEachPkgs (pkgs: import ./pkgs { inherit pkgs; });
    devShells = forEachPkgs (pkgs: import ./shell.nix { inherit pkgs; });
    formatter = forEachPkgs (pkgs: pkgs.nixpkgs-fmt);

    nixosConfigurations = {
      flakes = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs outputs; };
        modules = [ ./system/snow ];
      };
    };  

    homeConfigurations = {
      "Snow@flakes" = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages."x86_64-linux";
      extraSpecialArgs = { inherit inputs outputs; };
      modules = [ ./snow/flakes/home.nix ];
      };
    };

    homeConfigurations = {
      "Snow@drops" = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages."x86_64-linux";
      extraSpecialArgs = { inherit inputs outputs; };
      modules = [ ./snow/drops/home.nix ];
      };
    };

  };
}
