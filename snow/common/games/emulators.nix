{ pkgs, lib, ... }: 
{
  home.packages = with pkgs; [ 
    yuzu-early-access
    (retroarch.override {
      cores = with libretro; [
        snes9x
      ];
    })
  ];

  home.persistence = {
    "/persist/snow/flakes".directories = [ 
      ".local/share/yuzu"
      ".config/yuzu"
      "Games/Yuzu"
    ];
    "/persist/snow/flakes".allowOther = true;
  };
}
