{ config, pkgs, lib, ... }: 
{
  home.packages = with pkgs; [ anki ];

  home.persistence = {
    "/persist/snow/flakes".directories = [ 
      ".local/share/Anki"
      ".local/share/Anki2"
    ];
    "/persist/snow/flakes".allowOther = true;
  };
}
