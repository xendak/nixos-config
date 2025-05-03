{
  config,
  pkgs,
  ...
}: let
  inherit (config.colorscheme) palette;
in {
  home.file = {
    "/tmp/config.kv".source = pkgs.writeText "config.kv" ''
      #colors
      background=#${palette.base00}
      bg1=#${palette.base01}
      bgalt=4
      foreground=#${palette.base05}
      accent=#${palette.base0C}
      negative=#${palette.base08}
      button=#${palette.base01}
      inactive=#${palette.base03}
      link=#${palette.base0B}
      visitedlink=#${palette.base0E}

      save_folder="/home/${config.home.username}/.config/Kvantum"

      #details
      name=Nixos
      author="patch.sh utterly round plasma"
      comment="This comment will be put in kvconfig"
    '';
  };
}
