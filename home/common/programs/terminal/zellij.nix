{ pkgs, ... }:
{
  home.packages = [
    pkgs.zellij
  ];

  home.persistence = {
    "/persist" = {
      directories = [ ".config/zellij" ];
    };
  };
}
