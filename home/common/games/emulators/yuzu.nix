{ pkgs, config, inputs, ... }: 
{
  home.packages = [ inputs.suyu.packages.${pkgs.system}.suyu ];

  home.persistence = {
    "/persist/home/${config.home.username}" = {
      directories = [ 
        ".local/share/yuzu"
        ".config/yuzu"
        "Games/Emulators/Yuzu"
      ];
      allowOther = true;
    };
  };
}
