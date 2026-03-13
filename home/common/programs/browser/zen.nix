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

  zen-with-desktop = pkgs.symlinkJoin {
    name = "zen-browser-wrapped";
    paths = [ zen-wrapped ];

    postBuild = ''
      mkdir -p $out/share/applications

      if [ -f "${zen-pkg}/share/applications/zen.desktop" ]; then
        SRC_DESKTOP="${zen-pkg}/share/applications/zen.desktop"
      else
        SRC_DESKTOP="${zen-pkg}/share/applications/zen-beta.desktop"
      fi

      cp "$SRC_DESKTOP" $out/share/applications/zen.desktop
      chmod +w $out/share/applications/zen.desktop

      sed -i 's/^Exec=.*/Exec=zen %u/' $out/share/applications/zen.desktop
      sed -i 's/^Icon=.*/Icon=zen/' $out/share/applications/zen.desktop
    '';
  };
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
