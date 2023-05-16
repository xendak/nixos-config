{ inputs, lib, config, pkgs, ... }: {
  imports = [
#    ./common
#    ./common/wayland-wm
    inputs.hyprland.homeManagerModules.default
  ];
  programs = {
    fish.loginShellInit = ''
      if test (tty) = "/dev/tty1"
        exec Hyprland &> /dev/null
      end
    '';
  };

  home.packages = with pkgs; [
    inputs.hyprwm-contrib.packages.${system}.grimblast
    inputs.hyprpicker.packages.${pkgs.system}.hyprpicker
    inputs.hyprland-portal.packages.${pkgs.system}.xdg-desktop-portal-hyprland
    inputs.hyprland-portal.packages.${pkgs.system}.hyprland-share-picker
  ];

  programs.waybar.package = pkgs.waybar.overrideAttrs (oa: {
    mesonFlags = (oa.mesonFlags or [ ]) ++ [ "-Dexperimental=true" ];
  });

  wayland.windowManager.hyprland = {
    enable = true;  
    package = inputs.hyprland.packages.${pkgs.system}.hyprland-hidpi;
    extraConfig = 
      (import ./monitors.nix {
        inherit lib;
        inherit (config) monitors;
      }) +
      # +
      # builtins.replaceStrings ["#TRANSFORM"] 
      # [
      # 	( "monitor=DP-2,transform,3" )
      # ]
      (import ./config.nix {
        inherit (config) colorscheme;
        inherit config;
      });
  };
}
