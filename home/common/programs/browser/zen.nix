{
  config,
  pkgs,
  inputs,
  ...
}:

let
  zen-pkg = inputs.zen-browser.packages.${pkgs.stdenv.hostPlatform.system}.default;

  zen-wrapped = pkgs.writeShellScriptBin "zen" ''
    if [ -f "${zen-pkg}/bin/zen" ]; then
      BINARY="${zen-pkg}/bin/zen"
    else
      BINARY="${zen-pkg}/bin/zen-beta"
    fi

    exec "$BINARY" --profile "/home/${config.home.username}/.config/zen/${config.home.username}" "$@"
  '';

  zen-with-desktop = pkgs.runCommand "zen-with-desktop" { } ''
    mkdir -p $out/bin $out/share/applications

    ln -s ${zen-wrapped}/bin/zen $out/bin/zen

    if [ -f "${zen-pkg}/share/applications/zen.desktop" ]; then
      DESKTOP_FILE="${zen-pkg}/share/applications/zen.desktop"
    else
      DESKTOP_FILE="${zen-pkg}/share/applications/zen-beta.desktop"
    fi

    cp "$DESKTOP_FILE" $out/share/applications/zen.desktop

    substituteInPlace $out/share/applications/zen.desktop \
      --replace "Exec=zen" "Exec=$out/bin/zen" \
      --replace "Exec=zen-beta" "Exec=$out/bin/zen" \
      --replace "Icon=zen-beta" "Icon=zen"
  '';
in
{
  home.packages = [ zen-with-desktop ];

  home.persistence."/persist".directories = [
    ".config/zen/${config.home.username}"
  ];

  xdg.mimeApps.defaultApplications = {
    "text/html" = [ "zen.desktop" ];
    "text/xml" = [ "zen.desktop" ];
    "x-scheme-handler/http" = [ "zen.desktop" ];
    "x-scheme-handler/https" = [ "zen.desktop" ];
    "x-scheme-handler/about" = [ "zen.desktop" ];
    "x-scheme-handler/unknown" = [ "zen.desktop" ];
  };
}
