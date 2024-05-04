{ config, pkgs, ... }: 
{
  home.packages = with pkgs; [ rustdesk-flutter ];

  home.persistence = {
    "/persist/home/${config.home.username}" = {
        directories = [ 
          ".config/rustdesk"
      ];
      allowOther = true;
    };
  };
}
