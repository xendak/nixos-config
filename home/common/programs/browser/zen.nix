{
  config,
  pkgs,
  inputs,
  ...
}: let
  zen-wrapped = pkgs.writeShellScriptBin "zen" ''
    exec ${inputs.zen-browser.packages.${pkgs.system}.default}/bin/zen \
      --profile "/home/${config.home.username}/.config/zen/${config.home.username}" \
      "$@"
  '';

  zen-with-desktop = pkgs.runCommand "zen" {} ''
    mkdir -p $out/bin $out/share/applications
    ln -s ${zen-wrapped}/bin/zen $out/bin/zen
    cp ${inputs.zen-browser.packages.${pkgs.system}.default}/share/applications/zen.desktop $out/share/applications/zen.desktop
    substituteInPlace $out/share/applications/zen.desktop \
      --replace "Exec=zen-beta" "Exec=${zen-wrapped}/bin/zen"
  '';
in {
  home.packages = [
    zen-with-desktop
  ];
}
