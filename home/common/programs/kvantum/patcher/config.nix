{
  config,
  pkgs,
  ...
}: let
  inherit (config.colorscheme) c;
in {
  home.file = {
    "/tmp/config.kv".source = pkgs.writeText "config.kv" ''
      #colors
      background=#${c.base00}
      foreground=#${c.base05}
      accent=#${c.base0C}
      negative=#${c.base08}
      button=#${c.base01}
      inactive=#${c.base03}
      link=#${c.base0B}
      visitedlink=#${c.base0E}

      save_folder="/home/${config.home.username}/.config/kvantum"

      #details
      name=Nixos
      author="patch.sh utterly round plasma"
      comment="This comment will be put in kvconfig"
    '';
  };
}
