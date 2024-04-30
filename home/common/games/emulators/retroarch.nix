{ pkgs, config, ... }: 
{
  home.packages = with pkgs; [ 
    (retroarch.override {
      cores = with libretro; [
        snes9x
        mgba
      ];
    })
  ];

  home.persistence = {
    "/persist/home/${config.home.username}".directories = [ 
      ".local/share/retroarch"
      ".config/retroarch"
      "Games/Emulators/RetroArch"
    ];
    "/persist/home/${config.home.username}".allowOther = true;
  };
}
