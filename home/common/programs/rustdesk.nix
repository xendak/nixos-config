{ config, pkgs, ... }: 
{
  home.packages = with pkgs; [ rustdesk-flutter ];

  home.persistence = {
    "/persist/home/${config.home.username}" = {
        directories = [ 
          ".local/config/rustdesk"
      ];
      allowOther = true;
    };
  };
}
