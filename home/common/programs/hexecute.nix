{
  pkgs,
  inputs,
  ...
}:
{
  home.packages = [
    inputs.hexecute.packages.${pkgs.stdenv.hostPlatform.system}.default
  ];

  home.persistence = {
    "/persist" = {
      directories = [
        ".config/hexecute"
      ];
    };
  };
}
