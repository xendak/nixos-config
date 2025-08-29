{
  pkgs,
  config,
  inputs,
  ...
}:
{
  home.packages = [ inputs.suyu.packages.${pkgs.system}.suyu ];

  home.persistence = {
    "/persist" = {
      directories = [
        ".local/share/suyu"
        ".config/suyu"
        "Games/Suyu"
      ];
      allowOther = true;
    };
  };
}
