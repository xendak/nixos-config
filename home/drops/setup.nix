{ inputs, lib, pkgs, config, outputs, ... }:
let
  inherit (inputs.nix-colors) colorSchemes;
  # inherit (inputs.nix-colors.lib-contrib { inherit pkgs; }) colorschemeFromPicture nixWallpaperFromScheme;
  # sshKey = "/persist/home/secrets/agenix-${config.home.username}";
in
{
  imports = [
    inputs.impermanence.nixosModules.home-manager.impermanence
    inputs.nix-colors.homeManagerModule

    ../common/colors
  ] ++ (builtins.attrValues outputs.homeManagerModules);


  # age.secrets.pw.file = ../../secrets/pw.age;

  # # IdentityFile ${sshKey}
  # # home.file.".ssh/config".text = ''
  # #   Host github.com
  # #     IdentityFile ${config.age.secrets.github.path}
  # # '';
  # home.file = {
  #   ".ssh/known_hosts".source = ./known_hosts;
  #   ".ssh/id_ed25519.pub".source = ./id_ed25519.pub;
  #   ".ssh/id_ed25519".source = config.age.secrets.pw.path;
  # };

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
