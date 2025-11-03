{
  inputs,
  lib,
  pkgs,
  config,
  outputs,
  ...
}:
{
  imports = [
    inputs.impermanence.homeManagerModules.impermanence
    inputs.nix-colors.homeManagerModule

    ./common/colors
  ]
  ++ (builtins.attrValues outputs.homeManagerModules);

  nixpkgs = lib.mkIf (config.home.username == "nixos") {
    overlays = builtins.attrValues outputs.overlays;
    config = {
      allowUnfree = true;
      allowUnfreePredicate = _: true;
    };
  };

  home.file = {
    ".ssh/known_hosts".source = ./common/ssh/known_hosts;
    ".ssh/id_ed25519.pub".source = ./common/ssh/id_ed25519.pub;
    ".config/qmk/qmk.ini".source = pkgs.writeText "qmk.ini" ''
      [user]
      qmk_home = /home/${config.home.username}/Programming/qmk_userspace/qmk_firmware
    '';
    ".ssh/config".source = pkgs.writeText "config" ''
      AddKeysToAgent yes
    '';
    #https://github.com/hunkyburrito/xdg-desktop-portal-termfilechooser?tab=readme-ov-file#installation
    ".config/xdg-desktop-portal-termfilechooser/config".source = pkgs.writeText "config" ''
      [filechooser]
      cmd=${pkgs.xdg-desktop-portal-termfilechooser}/share/xdg-desktop-portal-termfilechooser/yazi-wrapper.sh
      default_dir=$HOME
      env=TERMCMD='${lib.getExe pkgs.foot} -a "f_yazi" -T "terminal filechooser"'

    '';
    ".config/xdg-desktop-portal/portals.conf".source = pkgs.writeText "portals.conf" ''
      [preferred]
      default=gnome;hyprland;gtk;kde
      org.freedesktop.impl.portal.FileChooser=termfilechooser
    '';
    ".config/fish/completions/ns.fish".source = pkgs.writeText "ns.fish" ''
      function __nixpkgs_completions
          cat ~/Flake/bin/nixpkgs_list
      end
      complete -c ns -f -a "(__nixpkgs_completions)"
    '';
    ".config/fish/completions/nix-run.fish".source = pkgs.writeText "nix-run.fish" ''
      function __nixpkgs_completions
          cat ~/Flake/bin/nixpkgs_list
      end
      complete -c nix-run -f -a "(__nixpkgs_completions)"
    '';
  };

  nix = {
    package = lib.mkDefault pkgs.nix;
    settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      warn-dirty = false;
    };
  };
}
