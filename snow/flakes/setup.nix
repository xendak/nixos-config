{ inputs, lib, pkgs, config, outputs, ... }:
let
  inherit (inputs.nix-colors) colorSchemes;
  # inherit (inputs.nix-colors.lib-contrib { inherit pkgs; }) colorschemeFromPicture nixWallpaperFromScheme;
  sshKey = "/persist/snow/secrets/agenix-${config.home.username}";
in
{
  imports = [
    inputs.impermanence.nixosModules.home-manager.impermanence
    inputs.nix-colors.homeManagerModule
    ../common/colors
  ] ++ (builtins.attrValues outputs.homeManagerModules);

  home.file.".ssh/config".text = ''
    Host github.com
      IdentityFile ${sshKey}
  '';

  nixpkgs = {
    overlays = builtins.attrValues outputs.overlays;
    config = {
      allowUnfree = true;
      allowUnfreePredicate = (_: true);
    };
  };

  nix = {
    package = lib.mkDefault pkgs.nix;
    settings = {
      experimental-features = [ "nix-command" "flakes" "repl-flake" ];
      warn-dirty = false;
    };
  };

  # Something? -------------
  colorscheme = lib.mkDefault colorSchemes.nord; #material-palenight; 
  
  home.file.".colorscheme".text = config.colorscheme.slug;

}
