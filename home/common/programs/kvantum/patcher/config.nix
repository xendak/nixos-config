{
  config,
  pkgs,
  ...
}: let
  inherit (config.colorscheme) colors;
in {
  home.file = {
    "/tmp/config.kv".source = pkgs.writeText "config.kv" ''
      #colors
      background=#${colors.base00}
      bg1=#${colors.base01}
      bgalt=4
      foreground=#${colors.base05}
      accent=#${colors.base0C}
      negative=#${colors.base08}
      button=#${colors.base01}
      inactive=#${colors.base03}
      link=#${colors.base0B}
      visitedlink=#${colors.base0E}

      save_folder="/home/${config.home.username}/.config/Kvantum"

      #details
      name=Nixos
      author="patch.sh utterly round plasma"
      comment="This comment will be put in kvconfig"
    '';
  };
}
