{
  pkgs,
  ...
}:
{
  home.packages = [
    pkgs.milton
  ];

  home.persistence = {
    "/persist" = {
      directories = [
        ".local/share/milton"
      ];
    };
  };
}
