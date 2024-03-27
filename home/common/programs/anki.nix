{ config, pkgs, lib, ... }: 
{
  home.packages = with pkgs; [ anki ];

  home.persistence = {
    "/persist/home/${config.home.username}" = {
        directories = [ 
          ".local/share/Anki"
          ".local/share/Anki2"
      ];
      allowOther = true;
    };
  };
}
