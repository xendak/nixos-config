{ pkgs, config, ... }: 
{
  home.packages = with pkgs; [ yuzu-early-access ];

  home.persistence = {
    "/persist/home/${config.home.username}".directories = [ 
      ".local/share/yuzu"
      ".config/yuzu"
      "Games/Emulators/Yuzu"
    ];
    "/persist/home/${config.home.username}".allowOther = true;
  };
}
