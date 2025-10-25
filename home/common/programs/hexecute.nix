{
  pkgs,
  inputs,
  ...
}:
{
  home.packages = [
    inputs.hexecute.packages.${pkgs.system}.default
  ];

  home.persistence = {
    "/persist" = {
      directories = [
        ".config/hexecute"
      ];
    };
  };
}
