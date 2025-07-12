{ config, pkgs, ... }:
{
  home.packages = [
    pkgs.pinentry-qt
  ];
  programs.rbw = {
    enable = true;
  };

  home.persistence = {
    "/persist/home/${config.home.username}" = {
      directories = [
        ".config/rbw"
        ".local/cache/rbw"
        ".local/share/rbw"
      ];
      allowOther = true;
    };
  };
}
