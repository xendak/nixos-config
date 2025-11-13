{ ... }:
{
  xdg.mimeApps.defaultApplications = {
    "application/pdf" = [ "org.pwmt.zathura.desktop" ];
  };

  programs.zathura.enable = true;
}
