{
  pkgs,
  ...
}:
{
  home.packages = [ pkgs.suyu ];

  home.persistence = {
    "/persist" = {
      directories = [
        ".local/share/suyu"
        ".config/suyu"
        "Games/Suyu"
      ];
    };
  };
}
