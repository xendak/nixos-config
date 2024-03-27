{ pkgs, config, ... }:
{
  imports = [
    ./plugins
  ];
  home.packages = with pkgs; [
    (nnn.override { withNerdIcons = true; })
  ];
}
