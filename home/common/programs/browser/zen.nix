{
  config,
  pkgs,
  inputs,
  ...
}:
let
  zen-wrapped = pkgs.writeShellScriptBin "zen" ''
    exec ${inputs.zen-browser.packages.${pkgs.stdenv.hostPlatform.system}.default}/bin/zen \
      --profile "/home/${config.home.username}/.config/zen/${config.home.username}" \
      "$@"
  '';

  zen-with-desktop = pkgs.runCommand "zen" { } ''
    mkdir -p $out/bin $out/share/applications


    # Check for zen first
    if [ -e "${zen-wrapped}/bin/zen" ]; then
      ln -s ${zen-wrapped}/bin/zen $out/bin/zen
    # If zen doesn't exist, check for zen-beta
    elif [ -e "${zen-wrapped}/bin/zen-beta" ]; then
      ln -s ${zen-wrapped}/bin/zen-beta $out/bin/zen
    else
      echo "Error: Neither zen nor zen-beta binary found in ${zen-wrapped}/bin/" >&2
      echo "Contents of ${zen-wrapped}/bin/:" >&2
      ls -la "${zen-wrapped}/bin/" | sed 's/^/  /' >&2
      exit 1
    fi

    # cp ${
      inputs.zen-browser.packages.${pkgs.stdenv.hostPlatform.system}.default
    }/share/applications/zen.desktop $out/share/applications/zen.desktop

    # Similar logic for desktop file
    if [ -e "${
      inputs.zen-browser.packages.${pkgs.stdenv.hostPlatform.system}.default
    }/share/applications/zen.desktop" ]; then
      cp ${
        inputs.zen-browser.packages.${pkgs.stdenv.hostPlatform.system}.default
      }/share/applications/zen.desktop $out/share/applications/zen.desktop
      substituteInPlace $out/share/applications/zen.desktop --replace "Exec=zen" "Exec=${zen-wrapped}/bin/zen"
      substituteInPlace $out/share/applications/zen.desktop --replace "Icon=zen-beta" "Icon=zen"
    elif [ -e "${
      inputs.zen-browser.packages.${pkgs.stdenv.hostPlatform.system}.default
    }/share/applications/zen-beta.desktop" ]; then
      cp ${
        inputs.zen-browser.packages.${pkgs.stdenv.hostPlatform.system}.default
      }/share/applications/zen-beta.desktop $out/share/applications/zen.desktop
      substituteInPlace $out/share/applications/zen.desktop --replace "Exec=zen-beta" "Exec=${zen-wrapped}/bin/zen"
      substituteInPlace $out/share/applications/zen.desktop --replace "Icon=zen-beta" "Icon=zen"
    else
      echo "Error: Neither zen.desktop nor zen-beta.desktop file found!" >&2
      echo "Contents of ${
        inputs.zen-browser.packages.${pkgs.stdenv.hostPlatform.system}.default
      }/share/applications/:" >&2
      ls -la "${
        inputs.zen-browser.packages.${pkgs.stdenv.hostPlatform.system}.default
      }/share/applications/" | sed 's/^/  /' >&2
      exit 1
    fi

  '';
in
{
  home.packages = [
    zen-with-desktop
  ];

  home.persistence = {
    "/persist" = {
      directories = [ ".config/zen/${config.home.username}" ];
    };
  };

  # set default since ungoogled chromium is giving me lots of issues with captcha
  xdg.mimeApps.defaultApplications = {
    "text/html" = [ "zen.desktop" ];
    "text/xml" = [ "zen.desktop" ];
    "x-scheme-handler/http" = [ "zen.desktop" ];
    "x-scheme-handler/https" = [ "zen.desktop" ];
    "x-scheme-handler/about" = [ "zen.desktop" ];
    "x-scheme-handler/unknown" = [ "zen.desktop" ];
  };
}
