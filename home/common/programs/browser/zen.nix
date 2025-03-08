{
  config,
  pkgs,
  inputs,
  ...
}: let
  # Create a wrapper script for the zen executable
  zen-wrapped = pkgs.writeShellScriptBin "zen" ''
    exec ${inputs.zen-browser.packages.${pkgs.system}.default}/bin/zen \
    --profile "/home/${config.home.username}/.config/zen/${config.home.username}" \
    "$@"
  '';
in {
  home.packages = [
    zen-wrapped
  ];
}
