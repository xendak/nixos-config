{ pkgs, lib, ... }: 
{
  home.packages = with pkgs; [ 
    yuzu-early-access
    mgba
    (retroarch.override {
      cores = with libretro; [
        snes9x
        mgba
      ];
    })
  ];

  home.persistence = {
    "/persist/home/${config.home.username}".directories = [ 
      ".local/share/yuzu"
      ".local/share/retroarch"
      ".config/retroarch"
      ".config/mgba"
      ".config/yuzu"
      "Games/Yuzu"
    ];
    "/persist/home/${config.home.username}".allowOther = true;
  };
}
