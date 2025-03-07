{pkgs, ...}: {
  # imports = [ ./plugins ];
  home.packages = with pkgs; [
    yazi
  ];

  # home.sessionVariables = { };
}

