{ pkgs, lib, ... }: 
{
  home.packages = with pkgs; [ yuzu-early-access ];

  home.persistence = {
    "/persist/snow/flakes".directories = [ 
      ".local/share/yuzu"
      ".config/yuzu"
      "Games/Yuzu"
    ];
    "/persist/snow/flakes".allowOther = true;
  };
}
