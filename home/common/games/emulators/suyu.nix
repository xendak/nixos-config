{
  pkgs,
  config,
  inputs,
  ...
}: {
  home.packages = [inputs.suyu.packages.${pkgs.system}.suyu];

  home.persistence = {
    "/persist/home/${config.home.username}" = {
      directories = [
        ".local/share/suyu"
        ".config/suyu"
        "Games/Suyu"
      ];
      allowOther = true;
    };
  };
}
