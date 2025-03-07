{pkgs, ...}: {
  xdg.configFile."vesktop/settings/quickCss.css".source =
    pkgs.writeText "quickCss.css"
    ''
    '';
}

