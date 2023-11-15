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
    "/persist/snow/flakes".directories = [ 
      ".local/share/yuzu"
      ".local/share/retroarch"
      ".config/retroarch"
      ".config/mgba"
      ".config/yuzu"
      "Games/Yuzu"
    ];
    "/persist/snow/flakes".allowOther = true;
  };
}
